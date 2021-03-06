#include <iostream>
#include <fstream>

#include <ros/ros.h>

#include <actionlib/client/simple_action_client.h>
#include <tabletop_object_detector/TabletopDetection.h>
#include <tabletop_collision_map_processing/TabletopCollisionMapProcessing.h>
#include <arm_navigation_msgs/SetPlanningSceneDiff.h>

#include <limits>
#include <household_objects_database_msgs/GetModelDescription.h>
#include <std_msgs/String.h>
#include <btBulletCollisionCommon.h>

#include <Yap/YapInterface.h>
#include <Yap/c_interface.h>

#include <kinematics_msgs/GetPositionFK.h>

#include <pr2_controllers_msgs/JointTrajectoryAction.h>
#include <pr2_controllers_msgs/Pr2GripperCommandAction.h>
#include <gazebo_msgs/BodyRequest.h>
#include <gazebo_msgs/ApplyBodyWrench.h>
#include <gazebo_msgs/SetModelConfiguration.h>
#include <pr2_mechanism_msgs/SwitchController.h>

#include <kinematics_msgs/GetKinematicSolverInfo.h>
#include <kinematics_msgs/GetPositionFK.h>
#include <kinematics_msgs/GetPositionIK.h>
#include <pcl/common/common.h>

#include <object_manipulator/tools/mechanism_interface.h>
#include <object_manipulator/tools/hand_description.h>
#include <object_manipulator/tools/service_action_wrappers.h>
#include <object_manipulator/tools/ik_tester_fast.h>
#include <object_manipulator/tools/vector_tools.h>
#include <object_manipulator/tools/arm_configurations.h>
#include <object_manipulation_msgs/ClusterBoundingBox.h>
#include <object_manipulation_msgs/tools.h>
#include <object_manipulation_msgs/PickupAction.h>
#include <object_manipulation_msgs/PlaceAction.h>
#include <manipulation_msgs/Grasp.h>
#include <arm_navigation_msgs/MoveArmAction.h>
#include <arm_navigation_msgs/utils.h>
#include <tf/transform_datatypes.h>
#include <tf/transform_listener.h>
#include <tf/tf.h>

#include <sensor_msgs/point_cloud_conversion.h>
#include <geometry_msgs/Quaternion.h>
#include <pr2_controllers_msgs/PointHeadAction.h>

#define ARRAY_SIZE(array) (sizeof((array))/sizeof((array[0])))
double particles = 200;

using object_manipulation_msgs::ManipulationResult;
using object_manipulator::InterruptRequestedException;

typedef actionlib::SimpleActionClient<pr2_controllers_msgs::PointHeadAction> PointHeadClient;
typedef actionlib::SimpleActionClient<
		pr2_controllers_msgs::Pr2GripperCommandAction> GripperClient;
using namespace std;

void sendQuery(string query) {
	cout << query << endl;
	YAP_Term error;
	const char *q2 = query.c_str();
	int res = YAP_RunGoal(YAP_ReadBuffer(q2, &error));
	if (res == 1)
			ROS_INFO("OK: query ok");
		else
			ROS_INFO("Error: query failed");
}

class LeftGripper {
private:
	GripperClient* client;
public:
	LeftGripper() {
		client = new GripperClient("l_gripper_controller/gripper_action", true);
		while (!client->waitForServer(ros::Duration(30.0))) {
			ROS_INFO("Waiting for l-gripper server");
		}
	}

	~LeftGripper() {
		delete client;
	}

	void open() {
		pr2_controllers_msgs::Pr2GripperCommandGoal open;
		open.command.position = 0.08;
		open.command.max_effort = -1;

		if (client->sendGoalAndWait(open, ros::Duration(10.0),
				ros::Duration(5.0))
				== actionlib::SimpleClientGoalState::SUCCEEDED)
			ROS_INFO("Gripper open");
		else
			ROS_INFO("Gripper could not open");
	}

};

class RightGripper {
private:
	GripperClient* client;
public:
	RightGripper() {
		client = new GripperClient("r_gripper_controller/gripper_action", true);
		while (!client->waitForServer(ros::Duration(30.0))) {
			ROS_INFO("Waiting for r-gripper server");
		}
	}

	~RightGripper() {
		delete client;
	}

	void open() {
		pr2_controllers_msgs::Pr2GripperCommandGoal open;
		open.command.position = 0.08;
		open.command.max_effort = -1;

		if (client->sendGoalAndWait(open, ros::Duration(10.0),
				ros::Duration(5.0))
				== actionlib::SimpleClientGoalState::SUCCEEDED)
			ROS_INFO("Gripper open");
		else
			ROS_INFO("Gripper could not open");
	}

};

class RobotHead {
private:
	PointHeadClient* point_head_client_;

public:
	RobotHead() {
		point_head_client_ = new PointHeadClient(
				"/head_traj_controller/point_head_action", true);
		while (!point_head_client_->waitForServer(ros::Duration(5.0))) {
			ROS_INFO("Waiting for the point_head_action server to come up");
		}
	}

	~RobotHead() {
		delete point_head_client_;
	}

	void lookAt(std::string frame_id, double x, double y, double z) {
		pr2_controllers_msgs::PointHeadGoal goal;
		geometry_msgs::PointStamped point;
		point.header.frame_id = frame_id;
		point.point.x = x;
		point.point.y = y;
		point.point.z = z;
		goal.target = point;

		goal.pointing_frame = "high_def_frame";
		goal.min_duration = ros::Duration(0.5);
		goal.max_velocity = 1.0;
		point_head_client_->sendGoal(goal);
		point_head_client_->waitForResult(ros::Duration(2));
	}

	void shakeHead(int n) {
		int count = 0;
		while (ros::ok() && ++count <= n) {
			lookAt("base_link", 5.0, 1.0, 1.2);
			lookAt("base_link", 5.0, -1.0, 1.2);
		}
	}

	void lookdown() {
		lookAt("base_link", 1, -0.25, 0);
	}

	tabletop_object_detector::TabletopDetection scanTable(ros::NodeHandle nh,
			ros::ServiceClient collision_processing_srv,
			ros::ServiceClient object_detection_srv) {

		tabletop_object_detector::TabletopDetection detection_call;
		detection_call.request.return_clusters = true;
		detection_call.request.return_models = true;
		detection_call.request.num_models = 1;
		if (!object_detection_srv.call(detection_call)) {
			ROS_ERROR("Tabletop detection service failed");
		}
		if (detection_call.response.detection.result
				!= detection_call.response.detection.SUCCESS) {
			ROS_ERROR(
					"Tabletop detection returned error code %d", detection_call.response.detection.result);
		}
		if (detection_call.response.detection.clusters.empty()
				&& detection_call.response.detection.models.empty()) {
			ROS_ERROR("The tabletop detector detected the table, "
			"but found no objects");
		}

		tabletop_collision_map_processing::TabletopCollisionMapProcessing processing_call;
		processing_call.request.detection_result =
				detection_call.response.detection;
		processing_call.request.reset_collision_models = true;
		processing_call.request.reset_attached_models = true;
		processing_call.request.desired_frame = "base_link";
		if (!collision_processing_srv.call(processing_call)) {
			ROS_ERROR("Collision map processing service failed");
		}
		if (processing_call.response.graspable_objects.empty()) {
			ROS_ERROR("Collision map processing returned no graspable objects");
		}
		return detection_call;
	}

};

class LeftArm {
private:

public:
	bool toSide;
	tf::Quaternion gripper_orientation;
	tf::Vector3 pickup_location;
	manipulation_msgs::Grasp pickupGrasp;

	LeftArm() {
		toSide = false;
		tfScalar x = 0;
		gripper_orientation.setX(x);
		tfScalar y = 0;
		gripper_orientation.setY(y);
		tfScalar z = 0;
		gripper_orientation.setZ(z);
		tfScalar w = 1;
		gripper_orientation.setW(w);
		gripper_orientation.normalize();
	}

	~LeftArm() {
		//delete move_arm_client;
	}

	void setPickupLocation(geometry_msgs::PoseStamped start_pose) {
		tfScalar x = start_pose.pose.position.x;
		tfScalar y = start_pose.pose.position.y;
		tfScalar z = start_pose.pose.position.z;
		pickup_location.setX(x);
		pickup_location.setY(y);
		pickup_location.setZ(z);
	}

	void setOrientation(geometry_msgs::PoseStamped start_pose) {
		tfScalar x = start_pose.pose.orientation.x;
		gripper_orientation.setX(x);
		tfScalar y = start_pose.pose.orientation.y;
		gripper_orientation.setY(y);
		tfScalar z = start_pose.pose.orientation.z;
		gripper_orientation.setZ(z);
		tfScalar w = start_pose.pose.orientation.w;
		gripper_orientation.setW(w);
		gripper_orientation.normalize();

		std::cout << "set orientation left" << std::endl;
		std::cout << gripper_orientation.getX() << std::endl;
		std::cout << gripper_orientation.getY() << std::endl;
		std::cout << gripper_orientation.getZ() << std::endl;
		std::cout << gripper_orientation.getW() << std::endl;

		double x2 = gripper_orientation.getX();
		double y2 = gripper_orientation.getY();
		double z2 = gripper_orientation.getZ();
		double w2 = gripper_orientation.getW();

		double magnitude2 = sqrt(
				pow(x2, 2.0) + pow(y2, 2.0) + pow(z2, 2.0) + pow(w2, 2.0));
	}

	void moveSide(ros::NodeHandle nh, double x, double y, double z, double w) {
		ROS_INFO("Moving left arm to the side");
		std::cout << x << std::endl;
						std::cout << y << std::endl;
						std::cout << z << std::endl;
						std::cout << w << std::endl;


		ros::ServiceClient get_state_client_ = nh.serviceClient<
				arm_navigation_msgs::GetRobotState>(
				"/environment_server/get_robot_state");
		arm_navigation_msgs::GetRobotState::Request req;
		arm_navigation_msgs::GetRobotState::Response res;

		get_state_client_.call(req, res);

		actionlib::SimpleActionClient<arm_navigation_msgs::MoveArmAction> move_arm2(
				"move_left_arm", true);
		move_arm2.waitForServer(ros::Duration(15.0));

		arm_navigation_msgs::MoveArmGoal goal;

		goal.motion_plan_request.group_name = "left_arm";
		goal.motion_plan_request.num_planning_attempts = 10;
		goal.motion_plan_request.planner_id = std::string("");
		goal.planner_service_name = std::string(
				"ompl_planning/plan_kinematic_path");
		goal.motion_plan_request.allowed_planning_time = ros::Duration(15.0);

		arm_navigation_msgs::SimplePoseConstraint desired_pose;
		desired_pose.header.frame_id = "base_link";
		desired_pose.link_name = "l_wrist_roll_link";
		desired_pose.pose.position.x = 0.35;//0.25
		desired_pose.pose.position.y = 0.75;//0.7
		desired_pose.pose.position.z = 1.05;

		desired_pose.pose.orientation.x = x;
		desired_pose.pose.orientation.y = y;
		desired_pose.pose.orientation.z = z;
		desired_pose.pose.orientation.w = w;

		desired_pose.absolute_position_tolerance.x = 0.3;
		desired_pose.absolute_position_tolerance.y = 0.3;
		desired_pose.absolute_position_tolerance.z = 0.3;

		desired_pose.absolute_roll_tolerance = 1;
		desired_pose.absolute_pitch_tolerance = 1;
		desired_pose.absolute_yaw_tolerance = 1;

		arm_navigation_msgs::addGoalConstraintToMoveArmGoal(desired_pose, goal);

		if (nh.ok()) {
			bool finished_within_time2 = false;
			move_arm2.sendGoal(goal);
			finished_within_time2 = move_arm2.waitForResult(
					ros::Duration(40.0));
			if (!finished_within_time2) {
				move_arm2.cancelGoal();
				ROS_INFO("Failed ideal pose, trying standard.");

				desired_pose.pose.orientation.x = x;
				desired_pose.pose.orientation.y = y;
				desired_pose.pose.orientation.z = z;
				desired_pose.pose.orientation.w = w;
				arm_navigation_msgs::addGoalConstraintToMoveArmGoal(
						desired_pose, goal);
				move_arm2.sendGoal(goal);
				move_arm2.waitForResult(ros::Duration(30.0));
				//TODO structuur en gripper loc update
			} else {
				/*
				actionlib::SimpleClientGoalState state2 = move_arm2.getState();
				bool success2 = (state2
						== actionlib::SimpleClientGoalState::SUCCEEDED);
				if (success2) {

					ROS_INFO("Action finished: %s", state2.toString().c_str());
				}

				else
					ROS_INFO("Action failed: %s", state2.toString().c_str());
				*/
				}
		}
		string qloc = "gripper_location(";
							qloc += "left_gripper";
							qloc += ",[";
							qloc += boost::lexical_cast<std::string>(
									desired_pose.pose.position.x);
							qloc += ",";
							qloc += boost::lexical_cast<std::string>(
									desired_pose.pose.position.y);
							qloc += ",";
							qloc += boost::lexical_cast<std::string>(
									desired_pose.pose.position.z);
							qloc += "],";
							qloc += boost::lexical_cast<std::string>(particles);
							qloc += ")";
							//cout << qloc << endl;
							sendQuery(qloc);
		toSide = true;

	}

	void moveSide(ros::NodeHandle nh) {
		double x2 = gripper_orientation.getX();
		double y2 = gripper_orientation.getY();
		double z2 = gripper_orientation.getZ();
		double w2 = gripper_orientation.getW();
		moveSide(nh, x2, y2, z2, w2);
	}

	void moveSideStandard(ros::NodeHandle nh) {
		moveSide(nh, 0, 0, 0, 1);
	}

	bool moveToLocation(ros::NodeHandle nh, double x, double y, double z) {
		double x2 = gripper_orientation.getX();
		//cout << x2 << endl;
		double y2 = gripper_orientation.getY();
		//cout << y2 << endl;
		double z2 = gripper_orientation.getZ();
		//cout << z2 << endl;
		double w2 = gripper_orientation.getW();
		//cout << w2 << endl;
		bool r = moveToLocation(nh, x, y, z, x2, y2, z2, w2);
		return r;
	}

	bool moveToLocation(ros::NodeHandle nh, double x, double y, double z,
			double x2, double y2, double z2, double w2) {
		/*
		 ros::ServiceClient get_state_client_ = nh.serviceClient<arm_navigation_msgs::GetRobotState>("/environment_server/get_robot_state");
		 arm_navigation_msgs::GetRobotState::Request req;
		 arm_navigation_msgs::GetRobotState::Response res;

		 get_state_client_.call(req,res);
		 */


		actionlib::SimpleActionClient<arm_navigation_msgs::MoveArmAction> move_arm2(
				"move_left_arm", true);
		move_arm2.waitForServer(ros::Duration(15.0));
		//move_arm2.cancelAllGoals();
		//move_arm2.getState().toString()
		//ROS_INFO("Connected to server");

		arm_navigation_msgs::MoveArmGoal goal;

		goal.motion_plan_request.group_name = "left_arm";
		goal.motion_plan_request.num_planning_attempts = 1;
		goal.motion_plan_request.planner_id = std::string("");
		goal.planner_service_name = std::string(
				"ompl_planning/plan_kinematic_path");
		goal.motion_plan_request.allowed_planning_time = ros::Duration(30.0);

		arm_navigation_msgs::SimplePoseConstraint desired_pose;
		desired_pose.header.frame_id = "base_link";
		desired_pose.link_name = "l_wrist_roll_link"; //l_wrist_roll_link
		desired_pose.pose.position.x = x;
		desired_pose.pose.position.y = y;
		desired_pose.pose.position.z = z;

		desired_pose.pose.orientation.x = x2;
		desired_pose.pose.orientation.y = y2;
		desired_pose.pose.orientation.z = z2;
		desired_pose.pose.orientation.w = w2; //start_pose.pose.orientation.w;

		desired_pose.absolute_position_tolerance.x = 0.2;
		desired_pose.absolute_position_tolerance.y = 0.2;
		desired_pose.absolute_position_tolerance.z = 0.2;

		desired_pose.absolute_roll_tolerance = 10;
		desired_pose.absolute_pitch_tolerance = 10;
		desired_pose.absolute_yaw_tolerance = 10;

		bool pathfound = false;
		if (nh.ok()) {

			arm_navigation_msgs::addGoalConstraintToMoveArmGoal(desired_pose,
					goal);
			bool finished_within_time2 = false;
			move_arm2.sendGoal(goal);
			finished_within_time2 = move_arm2.waitForResult(
					ros::Duration(20.0));
			if (!finished_within_time2) {
				move_arm2.cancelGoal();
				ROS_INFO("Timed out achieving goal A");
				pathfound = false;
			} else {
				actionlib::SimpleClientGoalState state2 = move_arm2.getState();
				bool success2 = (state2
						== actionlib::SimpleClientGoalState::SUCCEEDED);
				if (success2) {
					ROS_INFO("Action finished: %s", state2.toString().c_str());
					pathfound = true;
					string qloc = "gripper_location(";
					qloc += "left_gripper";
					qloc += ",[";
					qloc += boost::lexical_cast<std::string>(
							desired_pose.pose.position.x);
					qloc += ",";
					qloc += boost::lexical_cast<std::string>(
							desired_pose.pose.position.y);
					qloc += ",";
					qloc += boost::lexical_cast<std::string>(
							desired_pose.pose.position.z);
					qloc += "],";
					qloc += boost::lexical_cast<std::string>(particles);
					qloc += ")";

					//cout << qloc << endl;
					sendQuery(qloc);
				}

				else {
					move_arm2.cancelGoal();
				}

			}
		}
		toSide = true;
		return pathfound;
	}
	geometry_msgs::PoseStamped getPosition(ros::NodeHandle nh,
			std::string frame) {
		geometry_msgs::PoseStamped pose;

		std::cout << "get position procedure" << std::endl;
				std::cout << frame << std::endl;

		ros::ServiceClient get_state_client_ = nh.serviceClient<
				arm_navigation_msgs::GetRobotState>(
				"/environment_server/get_robot_state");
		arm_navigation_msgs::GetRobotState::Request req;
		arm_navigation_msgs::GetRobotState::Response res;
		get_state_client_.call(req, res);
		std::cout << res.error_code << std::endl;

		ros::service::waitForService("pr2_left_arm_kinematics/get_fk");
		ros::ServiceClient fk_client = nh.serviceClient<
				kinematics_msgs::GetPositionFK>(
				"pr2_left_arm_kinematics/get_fk");

		kinematics_msgs::GetPositionFK::Request fk_request;
		kinematics_msgs::GetPositionFK::Response fk_response;

		fk_request.header.frame_id = frame;
		fk_request.fk_link_names.resize(1);
		fk_request.fk_link_names[0] = "l_wrist_roll_link";
		fk_request.robot_state = res.robot_state;
		if (fk_client.call(fk_request, fk_response)) {
			//std::cout << fk_response.fk_link_names[0] << std::endl;
			//std::cout << fk_response.pose_stamped[0].header.frame_id
				//	<< std::endl;
			pose = fk_response.pose_stamped[0];
			printPose(pose);
		} else {
			//std::cout << "can't get link info" << std::endl;
		}
		printPose(pose);
		return pose;
	}

	void printPose(geometry_msgs::PoseStamped pose) {

		std::cout << "Wrist pose left currently:" << std::endl;
		std::cout << pose.pose.position.x << std::endl;
		std::cout << pose.pose.position.y << std::endl;
		std::cout << pose.pose.position.z << std::endl;
		std::cout << pose.pose.orientation.x << std::endl;
		std::cout << pose.pose.orientation.y << std::endl;
		std::cout << pose.pose.orientation.z << std::endl;
		std::cout << pose.pose.orientation.w << std::endl;

		}

	void place(
			tabletop_collision_map_processing::TabletopCollisionMapProcessing processing_call,
			int chosen, ros::NodeHandle nh,
			ros::ServiceClient collision_processing_srv,
			ros::ServiceClient object_detection_srv) {

		actionlib::SimpleActionClient<object_manipulation_msgs::PlaceAction> place_client(
				"/object_manipulator/object_manipulator_place", true);
		while (!place_client.waitForServer(ros::Duration(2.0)) && nh.ok()) {
			ROS_INFO_STREAM(
					"Waiting for action client " << "/object_manipulator/object_manipulator_place");
		}
		if (!nh.ok())
			exit(0);

		geometry_msgs::PoseStamped place_location;
		place_location.header.frame_id =
				processing_call.response.graspable_objects.at(0).reference_frame_id;
		place_location.pose.orientation.w = 1;
		place_location.header.stamp = ros::Time::now();
		place_location.pose.position.x = pickup_location.getX();
		place_location.pose.position.y = pickup_location.getY();
		place_location.pose.position.z = pickup_location.getZ();
		place_location.pose.orientation.x = gripper_orientation.getX();
		place_location.pose.orientation.y = gripper_orientation.getY();
		place_location.pose.orientation.z = gripper_orientation.getZ();
		place_location.pose.orientation.w = gripper_orientation.getW();

		/*
		cout << place_location.pose.orientation.x << endl;
		cout << place_location.pose.orientation.y << endl;
		cout << place_location.pose.orientation.z << endl;
		cout << place_location.pose.orientation.w << endl;

		cout << place_location.pose.position.x << endl;
		cout << place_location.pose.position.y << endl;
		cout << place_location.pose.position.z << endl;
	*/
		ROS_INFO("Calling the place action");
		object_manipulation_msgs::PlaceGoal place_goal;
		place_goal.place_locations.push_back(place_location);
		place_goal.collision_object_name =
				processing_call.response.collision_object_names.at(0);
		place_goal.collision_support_surface_name =
				processing_call.response.collision_support_surface_name;
		/*
		cout << processing_call.response.collision_support_surface_name << endl;
		cout
				<< processing_call.response.graspable_objects.at(0).__connection_header
				<< endl;
*/
		place_goal.grasp = pickupGrasp;
		place_goal.arm_name = "left_arm";
		place_goal.place_padding = 0.20;
		place_goal.desired_retreat_distance = 0.1;
		place_goal.min_retreat_distance = 0.05;

		geometry_msgs::Vector3Stamped direction;
		direction.header.stamp = ros::Time::now();
		direction.header.frame_id = "base_link";
		direction.vector.x = 0;
		direction.vector.y = 0;
		direction.vector.z = -1;
		place_goal.approach.direction = direction;
		place_goal.approach.desired_distance = 0.25;
		place_goal.approach.min_distance = 0.10;
		place_goal.use_reactive_place = false; //EXTENSIE
		place_client.sendGoal(place_goal);

		while (!place_client.waitForResult(ros::Duration(10.0))) {
			ROS_INFO("Waiting for the place action...");
		}
		object_manipulation_msgs::PlaceResult place_result =
				*(place_client.getResult());
		if (place_client.getState()
				!= actionlib::SimpleClientGoalState::SUCCEEDED) {
			//cout << place_client.getState().state_ << endl;
			//cout << place_client.getState().text_ << endl;
			ROS_ERROR(
					"Place failed with error code %d", place_result.manipulation_result.value);
			cout << place_result.manipulation_result.__connection_header
					<< endl;
		}
	}

	void pickup(
			tabletop_collision_map_processing::TabletopCollisionMapProcessing processing_call,
			int index_go, ros::NodeHandle nh) {
		actionlib::SimpleActionClient<object_manipulation_msgs::PickupAction> pickup_client(
				"/object_manipulator/object_manipulator_pickup", true);

		while (!pickup_client.waitForServer(ros::Duration(2.0)) && nh.ok()) {
			ROS_INFO_STREAM(
					"Waiting for action client " << "/object_manipulator/object_manipulator_pickup");
		}
		if (!nh.ok())
			exit(0);

		ROS_INFO("Calling the pickup action");
		object_manipulation_msgs::PickupGoal pickup_goal;
		pickup_goal.target = processing_call.response.graspable_objects.at(
				index_go);
		//pass the name that the object has in the collision environment
		//this name was also returned by the collision map processor
		pickup_goal.collision_object_name =
				processing_call.response.collision_object_names.at(index_go);
		pickup_goal.collision_support_surface_name =
				processing_call.response.collision_support_surface_name;
		pickup_goal.arm_name = "left_arm";
		geometry_msgs::Vector3Stamped direction;
		direction.header.stamp = ros::Time::now();
		direction.header.frame_id = "base_link";
		direction.vector.x = 0;
		direction.vector.y = 0;
		direction.vector.z = 1;
		pickup_goal.lift.direction = direction;
		pickup_goal.lift.desired_distance = 0.3;
		pickup_goal.lift.min_distance = 0.05;
		pickup_goal.use_reactive_lift = false;
		pickup_goal.use_reactive_execution = false;
		pickup_client.sendGoal(pickup_goal);
		while (!pickup_client.waitForResult(ros::Duration(10.0))) {
			ROS_INFO("Waiting for the pickup action...");
		}
		object_manipulation_msgs::PickupResult pickup_result =
				*(pickup_client.getResult());
		if (pickup_client.getState()
				!= actionlib::SimpleClientGoalState::SUCCEEDED) {
			ROS_ERROR(
					"The pickup action has failed with result code %d", pickup_result.manipulation_result.value);

		}
		pickupGrasp = pickup_result.grasp;
	}

	void pickup(
			tabletop_collision_map_processing::TabletopCollisionMapProcessing processing_call,
			int chosen, ros::NodeHandle nh,
			ros::ServiceClient collision_processing_srv,
			ros::ServiceClient object_detection_srv) {

		actionlib::SimpleActionClient<object_manipulation_msgs::PickupAction> pickup_client(
				"/object_manipulator/object_manipulator_pickup", true);
		while (!ros::service::waitForService("/object_detection",
				ros::Duration(2.0)) && nh.ok()) {
			ROS_INFO("Waiting for object detection service to come up");
		}
		if (!nh.ok())
			exit(0);
		object_detection_srv = nh.serviceClient<
				tabletop_object_detector::TabletopDetection>(
				"/object_detection", true);

		while (!ros::service::waitForService(
				"/tabletop_collision_map_processing/tabletop_collision_map_processing",
				ros::Duration(2.0)) && nh.ok()) {
			ROS_INFO("Waiting for collision processing service to come up");
		}
		if (!nh.ok())
			exit(0);
		collision_processing_srv =
				nh.serviceClient<
						tabletop_collision_map_processing::TabletopCollisionMapProcessing>(
						"/tabletop_collision_map_processing/tabletop_collision_map_processing",
						true);

		while (!pickup_client.waitForServer(ros::Duration(2.0)) && nh.ok()) {
			ROS_INFO_STREAM(
					"Waiting for action client " << "/object_manipulator/object_manipulator_pickup");
		}
		if (!nh.ok())
			exit(0);

		ROS_INFO("Calling the pickup action");
		object_manipulation_msgs::PickupGoal pickup_goal;
		pickup_goal.target = processing_call.response.graspable_objects.at(
				chosen);
		pickup_goal.collision_object_name =
				processing_call.response.collision_object_names.at(chosen);
		pickup_goal.collision_support_surface_name =
				processing_call.response.collision_support_surface_name;
		pickup_goal.arm_name = "left_arm";
		geometry_msgs::Vector3Stamped direction;
		direction.header.stamp = ros::Time::now();
		direction.header.frame_id = "base_link";
		direction.vector.x = 0;
		direction.vector.y = 0;
		direction.vector.z = 1;
		pickup_goal.lift.direction = direction;
		pickup_goal.lift.desired_distance = 0.3;
		pickup_goal.lift.min_distance = 0.3;
		pickup_goal.use_reactive_lift = false;
		pickup_goal.use_reactive_execution = false;
		pickup_client.sendGoal(pickup_goal);
		while (!pickup_client.waitForResult(ros::Duration(30.0))) {
			ROS_INFO("Waiting for the pickup action...");
		}
		object_manipulation_msgs::PickupResult pickup_result =
				*(pickup_client.getResult());
		if (pickup_client.getState()
				!= actionlib::SimpleClientGoalState::SUCCEEDED) {
			ROS_ERROR(
					"The pickup action has failed with result code %d", pickup_result.manipulation_result.value);

		}
		pickupGrasp = pickup_result.grasp;
	}
};

class RightArm {
private:

public:
	bool toSide;
	tf::Quaternion gripper_orientation;
	tf::Vector3 pickup_location;
	manipulation_msgs::Grasp pickupGrasp;

	RightArm() {
		toSide = false;

		tfScalar x = 0;
		gripper_orientation.setX(x);
		tfScalar y = 0;
		gripper_orientation.setY(y);
		tfScalar z = 0;
		gripper_orientation.setZ(z);
		tfScalar w = 1;

		gripper_orientation.setW(w);
		gripper_orientation.normalize();
	}

	~RightArm() {
		//delete move_arm_client;
	}

	void setPickupLocation(geometry_msgs::PoseStamped start_pose) {
		tfScalar x = start_pose.pose.position.x;
		tfScalar y = start_pose.pose.position.y;
		tfScalar z = start_pose.pose.position.z;
		pickup_location.setX(x);
		pickup_location.setY(y);
		pickup_location.setZ(z);
	}

	void setOrientation(geometry_msgs::PoseStamped start_pose) {
		tfScalar x = start_pose.pose.orientation.x;
		gripper_orientation.setX(x);
		tfScalar y = start_pose.pose.orientation.y;
		gripper_orientation.setY(y);
		tfScalar z = start_pose.pose.orientation.z;
		gripper_orientation.setZ(z);
		tfScalar w = start_pose.pose.orientation.w;
		gripper_orientation.setW(w);
		gripper_orientation.normalize();
		std::cout << "set orientation right" << std::endl;
				std::cout << gripper_orientation.getX() << std::endl;
				std::cout << gripper_orientation.getY() << std::endl;
				std::cout << gripper_orientation.getZ() << std::endl;
				std::cout << gripper_orientation.getW() << std::endl;
		double x2 = gripper_orientation.getX();
		double y2 = gripper_orientation.getY();
		double z2 = gripper_orientation.getZ();
		double w2 = gripper_orientation.getW();

		double magnitude2 = sqrt(
				pow(x2, 2.0) + pow(y2, 2.0) + pow(z2, 2.0) + pow(w2, 2.0));
		//std::cout << magnitude2 << std::endl;
	}

	void moveSide(ros::NodeHandle nh, double x, double y, double z, double w) {
		ROS_INFO("Moving right arm to the side");
		std::cout << x << std::endl;
						std::cout << y << std::endl;
						std::cout << z << std::endl;
						std::cout << w << std::endl;


		ros::ServiceClient get_state_client_ = nh.serviceClient<
				arm_navigation_msgs::GetRobotState>(
				"/environment_server/get_robot_state");
		arm_navigation_msgs::GetRobotState::Request req;
		arm_navigation_msgs::GetRobotState::Response res;

		get_state_client_.call(req, res);

		actionlib::SimpleActionClient<arm_navigation_msgs::MoveArmAction> move_arm2(
				"move_right_arm", true);
		move_arm2.waitForServer(ros::Duration(15.0));

		arm_navigation_msgs::MoveArmGoal goal;

		goal.motion_plan_request.group_name = "right_arm";
		goal.motion_plan_request.num_planning_attempts = 10;
		goal.motion_plan_request.planner_id = std::string("");
		goal.planner_service_name = std::string(
				"ompl_planning/plan_kinematic_path");
		goal.motion_plan_request.allowed_planning_time = ros::Duration(15.0);

		arm_navigation_msgs::SimplePoseConstraint desired_pose;
		desired_pose.header.frame_id = "base_link";
		desired_pose.link_name = "r_wrist_roll_link";
		desired_pose.pose.position.x = 0.30;
		desired_pose.pose.position.y = -0.75;
		desired_pose.pose.position.z = 1.05;

		desired_pose.pose.orientation.x = x;
		desired_pose.pose.orientation.y = y;
		desired_pose.pose.orientation.z = z;
		desired_pose.pose.orientation.w = w;

		desired_pose.absolute_position_tolerance.x = 0.3;
		desired_pose.absolute_position_tolerance.y = 0.3;
		desired_pose.absolute_position_tolerance.z = 0.3;

		desired_pose.absolute_roll_tolerance = 1;
		desired_pose.absolute_pitch_tolerance = 1;
		desired_pose.absolute_yaw_tolerance = 1;

		arm_navigation_msgs::addGoalConstraintToMoveArmGoal(desired_pose, goal);

		if (nh.ok()) {
			bool finished_within_time2 = false;
			move_arm2.sendGoal(goal);
			finished_within_time2 = move_arm2.waitForResult(
					ros::Duration(40.0));
			if (!finished_within_time2) {
				move_arm2.cancelGoal();
				ROS_INFO("Failed ideal pose, trying standard.");

				desired_pose.pose.orientation.x = x;
				desired_pose.pose.orientation.y = y;
				desired_pose.pose.orientation.z = z;
				desired_pose.pose.orientation.w = w;
				arm_navigation_msgs::addGoalConstraintToMoveArmGoal(
						desired_pose, goal);
				move_arm2.sendGoal(goal);
				move_arm2.waitForResult(ros::Duration(30.0));
			} else {
				/*
				actionlib::SimpleClientGoalState state2 = move_arm2.getState();
				bool success2 = (state2
						== actionlib::SimpleClientGoalState::SUCCEEDED);
				if (success2) {

					ROS_INFO("Action finished: %s", state2.toString().c_str());
				}

				else
					ROS_INFO("Action failed: %s", state2.toString().c_str());
				*/
				}

		}
		string qloc = "gripper_location(";
							qloc += "right_gripper";
							qloc += ",[";
							qloc += boost::lexical_cast<std::string>(
									desired_pose.pose.position.x);
							qloc += ",";
							qloc += boost::lexical_cast<std::string>(
									desired_pose.pose.position.y);
							qloc += ",";
							qloc += boost::lexical_cast<std::string>(
									desired_pose.pose.position.z);
							qloc += "],";
							qloc += boost::lexical_cast<std::string>(particles);
							qloc += ")";

							//cout << qloc << endl;
							sendQuery(qloc);
		toSide = true;
	}

	void moveSide(ros::NodeHandle nh) {
		double x2 = gripper_orientation.getX();
		double y2 = gripper_orientation.getY();
		double z2 = gripper_orientation.getZ();
		double w2 = gripper_orientation.getW();
		moveSide(nh, x2, y2, z2, w2);
	}

	void moveSideStandard(ros::NodeHandle nh) {
		moveSide(nh, 0, 0, 0, 1);
	}

	bool moveToLocation(ros::NodeHandle nh, double x, double y, double z) {
		double x2 = gripper_orientation.getX();
		double y2 = gripper_orientation.getY();
		double z2 = gripper_orientation.getZ();
		double w2 = gripper_orientation.getW();
		bool success = moveToLocation(nh, x, y, z, x2, y2, z2, w2);
		return success;
	}

	bool moveToLocation(ros::NodeHandle nh, double x, double y, double z,
			double x2, double y2, double z2, double w2) {



		actionlib::SimpleActionClient<arm_navigation_msgs::MoveArmAction> move_arm2(
				"move_right_arm", true);
		move_arm2.waitForServer(ros::Duration(15.0));
		ROS_INFO("Connected to server");

		arm_navigation_msgs::MoveArmGoal goal;

		goal.motion_plan_request.group_name = "right_arm";
		goal.motion_plan_request.num_planning_attempts = 1;
		goal.motion_plan_request.planner_id = std::string("");
		goal.planner_service_name = std::string(
				"ompl_planning/plan_kinematic_path");
		goal.motion_plan_request.allowed_planning_time = ros::Duration(15.0);

		arm_navigation_msgs::SimplePoseConstraint desired_pose;
		desired_pose.header.frame_id = "base_link";
		desired_pose.link_name = "r_wrist_roll_link";
		desired_pose.pose.position.x = x;
		desired_pose.pose.position.y = y;
		desired_pose.pose.position.z = z;//TODO +0.2

		desired_pose.pose.orientation.x = x2;
		desired_pose.pose.orientation.y = y2;
		desired_pose.pose.orientation.z = z2;
		desired_pose.pose.orientation.w = w2;

		desired_pose.absolute_position_tolerance.x = 0.05;
		desired_pose.absolute_position_tolerance.y = 0.05;
		desired_pose.absolute_position_tolerance.z = 0.01;

		desired_pose.absolute_roll_tolerance = 1;
		desired_pose.absolute_pitch_tolerance = 1;
		desired_pose.absolute_yaw_tolerance = 1;

		arm_navigation_msgs::addGoalConstraintToMoveArmGoal(desired_pose, goal);

		bool success2;
		if (nh.ok()) {
			bool finished_within_time2 = false;
			//SimpleCli= move_arm2.getState();
			move_arm2.sendGoal(goal);
			finished_within_time2 = move_arm2.waitForResult(
					ros::Duration(20.0));
			if (!finished_within_time2) {
				move_arm2.cancelGoal();
				ROS_INFO("Timed out achieving goal A");
			} else {
				actionlib::SimpleClientGoalState state2 = move_arm2.getState();
				success2 = (state2
						== actionlib::SimpleClientGoalState::SUCCEEDED);
				if (success2) {
					string qloc = "gripper_location(";
					qloc += "right_gripper";
					qloc += ",[";
					qloc += boost::lexical_cast<std::string>(
							desired_pose.pose.position.x);
					qloc += ",";
					qloc += boost::lexical_cast<std::string>(
							desired_pose.pose.position.y);
					qloc += ",";
					qloc += boost::lexical_cast<std::string>(
							desired_pose.pose.position.z);
					qloc += "],";
					qloc += boost::lexical_cast<std::string>(particles);
					qloc += ")";

					//cout << qloc << endl;
					sendQuery(qloc);
					ROS_INFO("Action finished: %s", state2.toString().c_str());
				}

				else
					ROS_INFO("Action failed: %s", state2.toString().c_str());
			}
		}
		toSide = true;
		return success2;
	}

	void printPose(geometry_msgs::PoseStamped pose) {

		std::cout << "Wrist pose right currently:" << std::endl;
		std::cout << pose.pose.position.x << std::endl;
		std::cout << pose.pose.position.y << std::endl;
		std::cout << pose.pose.position.z << std::endl;
		std::cout << pose.pose.orientation.x << std::endl;
		std::cout << pose.pose.orientation.y << std::endl;
		std::cout << pose.pose.orientation.z << std::endl;
		std::cout << pose.pose.orientation.w << std::endl;

	}

	void place(
			tabletop_collision_map_processing::TabletopCollisionMapProcessing processing_call,
			int chosen, ros::NodeHandle nh,
			ros::ServiceClient collision_processing_srv,
			ros::ServiceClient object_detection_srv) {

		actionlib::SimpleActionClient<object_manipulation_msgs::PlaceAction> place_client(
				"/object_manipulator/object_manipulator_place", true);
		while (!place_client.waitForServer(ros::Duration(2.0)) && nh.ok()) {
			ROS_INFO_STREAM(
					"Waiting for action client " << "/object_manipulator/object_manipulator_place");
		}
		if (!nh.ok())
			exit(0);

		geometry_msgs::PoseStamped place_location;
		place_location.header.frame_id =
				processing_call.response.graspable_objects.at(0).reference_frame_id;
		place_location.pose.orientation.w = 1;
		place_location.header.stamp = ros::Time::now();
		place_location.pose.position.x += 0.1;

		ROS_INFO("Calling the place action");
		object_manipulation_msgs::PlaceGoal place_goal;
		place_goal.place_locations.push_back(place_location);
		place_goal.collision_object_name =
				processing_call.response.collision_object_names.at(0);
		place_goal.collision_support_surface_name =
				processing_call.response.collision_support_surface_name;

		place_goal.grasp = pickupGrasp;
		place_goal.arm_name = "right_arm";
		place_goal.place_padding = 0.03;
		place_goal.desired_retreat_distance = 0.1;
		place_goal.min_retreat_distance = 0.05;

		geometry_msgs::Vector3Stamped direction;
		direction.header.stamp = ros::Time::now();
		direction.header.frame_id = "base_link";
		direction.vector.x = 0;
		direction.vector.y = 0;
		direction.vector.z = -1;
		place_goal.approach.direction = direction;
		place_goal.approach.desired_distance = 0.08;
		place_goal.approach.min_distance = 0.06;
		place_goal.use_reactive_place = false;
		place_client.sendGoal(place_goal);

		while (!place_client.waitForResult(ros::Duration(10.0))) {
			ROS_INFO("Waiting for the place action...");
		}
		object_manipulation_msgs::PlaceResult place_result =
				*(place_client.getResult());
		if (place_client.getState()
				!= actionlib::SimpleClientGoalState::SUCCEEDED) {
			ROS_ERROR(
					"Place failed with error code %d", place_result.manipulation_result.value);
		}
	}

	void pickup(
			tabletop_collision_map_processing::TabletopCollisionMapProcessing processing_call,
			int index_go, ros::NodeHandle nh) {
		actionlib::SimpleActionClient<object_manipulation_msgs::PickupAction> pickup_client(
				"/object_manipulator/object_manipulator_pickup", true);

		while (!pickup_client.waitForServer(ros::Duration(2.0)) && nh.ok()) {
			ROS_INFO_STREAM(
					"Waiting for action client " << "/object_manipulator/object_manipulator_pickup");
		}
		if (!nh.ok())
			exit(0);

		ROS_INFO("Calling the pickup action");
		object_manipulation_msgs::PickupGoal pickup_goal;
		pickup_goal.target = processing_call.response.graspable_objects.at(
				index_go);
		//pass the name that the object has in the collision environment
		//this name was also returned by the collision map processor
		pickup_goal.collision_object_name =
				processing_call.response.collision_object_names.at(index_go);
		pickup_goal.collision_support_surface_name =
				processing_call.response.collision_support_surface_name;
		pickup_goal.arm_name = "right_arm";
		geometry_msgs::Vector3Stamped direction;
		direction.header.stamp = ros::Time::now();
		direction.header.frame_id = "base_link";
		direction.vector.x = 0;
		direction.vector.y = 0;
		direction.vector.z = 1;
		pickup_goal.lift.direction = direction;
		pickup_goal.lift.desired_distance = 0.3;
		pickup_goal.lift.min_distance = 0.05;
		pickup_goal.use_reactive_lift = false;
		pickup_goal.use_reactive_execution = false;
		pickup_client.sendGoal(pickup_goal);
		while (!pickup_client.waitForResult(ros::Duration(10.0))) {
			ROS_INFO("Waiting for the pickup action...");
		}
		object_manipulation_msgs::PickupResult pickup_result =
				*(pickup_client.getResult());
		if (pickup_client.getState()
				!= actionlib::SimpleClientGoalState::SUCCEEDED) {
			ROS_ERROR(
					"The pickup action has failed with result code %d", pickup_result.manipulation_result.value);

		}
		pickupGrasp = pickup_result.grasp;
	}

	void pickup(
			tabletop_collision_map_processing::TabletopCollisionMapProcessing processing_call,
			int chosen, ros::NodeHandle nh,
			ros::ServiceClient collision_processing_srv,
			ros::ServiceClient object_detection_srv) {
/*
		cout
				<< processing_call.response.graspable_objects[chosen].collision_name
				<< endl;
	*/
		actionlib::SimpleActionClient<object_manipulation_msgs::PickupAction> pickup_client(
				"/object_manipulator/object_manipulator_pickup", true);
		while (!ros::service::waitForService("/object_detection",
				ros::Duration(2.0)) && nh.ok()) {
			ROS_INFO("Waiting for object detection service to come up");
		}
		if (!nh.ok())
			exit(0);
		object_detection_srv = nh.serviceClient<
				tabletop_object_detector::TabletopDetection>(
				"/object_detection", true);

		while (!ros::service::waitForService(
				"/tabletop_collision_map_processing/tabletop_collision_map_processing",
				ros::Duration(2.0)) && nh.ok()) {
			ROS_INFO("Waiting for collision processing service to come up");
		}
		if (!nh.ok())
			exit(0);
		collision_processing_srv =
				nh.serviceClient<
						tabletop_collision_map_processing::TabletopCollisionMapProcessing>(
						"/tabletop_collision_map_processing/tabletop_collision_map_processing",
						true);

		while (!pickup_client.waitForServer(ros::Duration(2.0)) && nh.ok()) {
			ROS_INFO_STREAM(
					"Waiting for action client " << "/object_manipulator/object_manipulator_pickup");
		}
		if (!nh.ok())
			exit(0);

		ROS_INFO("Calling the pickup action");
		object_manipulation_msgs::PickupGoal pickup_goal;
		pickup_goal.target = processing_call.response.graspable_objects.at(
				chosen);
		pickup_goal.collision_object_name =
				processing_call.response.collision_object_names.at(chosen);
		pickup_goal.collision_support_surface_name =
				processing_call.response.collision_support_surface_name;
		pickup_goal.arm_name = "right_arm";
		geometry_msgs::Vector3Stamped direction;
		direction.header.stamp = ros::Time::now();
		direction.header.frame_id = "base_link";
		direction.vector.x = 0;
		direction.vector.y = 0;
		direction.vector.z = 1;
		pickup_goal.lift.direction = direction;
		pickup_goal.lift.desired_distance = 0.3;
		pickup_goal.lift.min_distance = 0.3;
		pickup_goal.use_reactive_lift = false;
		pickup_goal.use_reactive_execution = false;
		pickup_client.sendGoal(pickup_goal);
		while (!pickup_client.waitForResult(ros::Duration(30.0))) {
			ROS_INFO("Waiting for the pickup action...");
		}
		object_manipulation_msgs::PickupResult pickup_result =
				*(pickup_client.getResult());
		if (pickup_client.getState()
				!= actionlib::SimpleClientGoalState::SUCCEEDED) {
			ROS_ERROR(
					"The pickup action has failed with result code %d", pickup_result.manipulation_result.value);

		}
		pickupGrasp = pickup_result.grasp;
	}

	geometry_msgs::PoseStamped getPosition(ros::NodeHandle nh,
			std::string frame) {

		std::cout << "get position procedure" << std::endl;
		std::cout << frame << std::endl;
		geometry_msgs::PoseStamped pose;

		ros::ServiceClient get_state_client_ = nh.serviceClient<
				arm_navigation_msgs::GetRobotState>(
				"/environment_server/get_robot_state");
		arm_navigation_msgs::GetRobotState::Request req;
		arm_navigation_msgs::GetRobotState::Response res;
		get_state_client_.call(req, res);
		std::cout << res.error_code << std::endl;

		ros::service::waitForService("pr2_right_arm_kinematics/get_fk");
		ros::ServiceClient fk_client = nh.serviceClient<
				kinematics_msgs::GetPositionFK>(
				"pr2_right_arm_kinematics/get_fk");

		kinematics_msgs::GetPositionFK::Request fk_request;
		kinematics_msgs::GetPositionFK::Response fk_response;

		fk_request.header.frame_id = frame;
		fk_request.fk_link_names.resize(1);
		fk_request.fk_link_names[0] = "r_wrist_roll_link";
		fk_request.robot_state = res.robot_state;
		if (fk_client.call(fk_request, fk_response)) {
			std::cout << fk_response.fk_link_names[0] << std::endl;
			std::cout << fk_response.pose_stamped[0].header.frame_id;
			pose = fk_response.pose_stamped[0];
			printPose(pose);
		} else {
			std::cout << "can't get link info" << std::endl;
		}
		printPose(pose);
		return pose;
	}

};

using namespace std;
/*
 namespace pr2_interactive_manipulation {
 typedef actionlib::SimpleActionClient<pr2_object_manipulation_msgs::IMGUIAction> Client;
 }
 */
const std::string OBJECT_DETECTION_SERVICE_NAME = "/object_detection";

int toInt(string bitString, int sLength) {

	int tempInt;
	int num = 0;
	for (int i = 0; i < sLength; i++) {
		tempInt = bitString[i] - '0';
		num |= (1 << (sLength - 1 - i)) * tempInt;
	}

	return num;
}

void table_info(tabletop_object_detector::TabletopDetection detection_call) {
	cout << "Table info" << endl;
	cout << detection_call.response.detection.table.pose.header.frame_id
			<< endl;
	cout << detection_call.response.detection.table.pose.header.stamp << endl;
	cout << detection_call.response.detection.table.pose.header.seq << endl;
	cout << endl;

	int nvert =
			detection_call.response.detection.table.convex_hull.vertices.size();
	cout << "Number of table vertices: " << nvert;
	cout << endl;
	for (int v = 0; v < nvert; v++) {
		cout
				<< detection_call.response.detection.table.convex_hull.vertices[v].x
				<< endl;
		cout
				<< detection_call.response.detection.table.convex_hull.vertices[v].y
				<< endl;
		cout
				<< detection_call.response.detection.table.convex_hull.vertices[v].z
				<< endl;

		cout << endl;
	}
	cout << endl;

	cout << "Table position" << endl;
	cout << detection_call.response.detection.table.pose.pose.position.x
			<< endl;
	float x = detection_call.response.detection.table.pose.pose.position.x;
	cout << detection_call.response.detection.table.pose.pose.position.y
			<< endl;
	float y = detection_call.response.detection.table.pose.pose.position.y;
	cout << detection_call.response.detection.table.pose.pose.position.z
			<< endl;
	float z = detection_call.response.detection.table.pose.pose.position.z;
	cout << endl;

	cout << "Table orientation" << endl;
	cout << detection_call.response.detection.table.pose.pose.orientation.x
			<< endl;
	cout << detection_call.response.detection.table.pose.pose.orientation.y
			<< endl;
	cout << detection_call.response.detection.table.pose.pose.orientation.z
			<< endl;
	cout << detection_call.response.detection.table.pose.pose.orientation.w
			<< endl;

	float xmin1 = detection_call.response.detection.table.x_min;
	float xmin2 = xmin1 + x;

	float xmax1 = detection_call.response.detection.table.x_max;
	float xmax2 = xmax1 + x;

	float ymin1 = detection_call.response.detection.table.y_min;
	float ymin2 = ymin1 + y;

	float ymax1 = detection_call.response.detection.table.y_max;
	float ymax2 = ymax1 + y;

	float zmin2 = z - 0.02;
	float zmax2 = z;

	cout << "Table boundaries" << endl;
	cout << xmin1 << endl;
	cout << xmax1 << endl;
	cout << ymin1 << endl;
	cout << ymax1 << endl;

	cout << endl;
	cout << "Final boundaries:" << endl;
	cout << xmin2 << endl;
	cout << xmax2 << endl;
	cout << ymin2 << endl;
	cout << ymax2 << endl;
	cout << zmin2 << endl;
	cout << zmax2 << endl;

}

void clusterObsBBInit(
		tabletop_object_detector::TabletopDetection detection_call, int i,
		int npoints) {
	float xmin = std::numeric_limits<float>::max();
	float xmax = std::numeric_limits<float>::min();
	float ymin = std::numeric_limits<float>::max();
	float ymax = std::numeric_limits<float>::min();
	float zmin = std::numeric_limits<float>::max();
	float zmax = std::numeric_limits<float>::min();
	for (int j = 0; j < npoints; j++) {
		if (detection_call.response.detection.clusters[i].points[j].x < xmin) {
			xmin = detection_call.response.detection.clusters[i].points[j].x;
		}
		if (detection_call.response.detection.clusters[i].points[j].x > xmax) {
			xmax = detection_call.response.detection.clusters[i].points[j].x;
		}
		if (detection_call.response.detection.clusters[i].points[j].y < ymin) {
			ymin = detection_call.response.detection.clusters[i].points[j].y;
		}
		if (detection_call.response.detection.clusters[i].points[j].y > ymax) {
			ymax = detection_call.response.detection.clusters[i].points[j].y;
		}
		if (detection_call.response.detection.clusters[i].points[j].z < zmin) {
			zmin = detection_call.response.detection.clusters[i].points[j].z;
		}
		if (detection_call.response.detection.clusters[i].points[j].z > zmax) {
			zmax = detection_call.response.detection.clusters[i].points[j].z;
		}
	}
	float xmid = (xmin + xmax) / 2;
	float ymid = (ymin + ymax) / 2;
	float zmid = (zmin + zmax) / 2;

	string q = "locationObservationInit(";
	q += boost::lexical_cast<std::string>(i + 1);
	q += ",[";
	q += boost::lexical_cast<std::string>(abs(xmin - xmax));
	q += ",";
	q += boost::lexical_cast<std::string>(abs(ymin - ymax));
	q += ",";
	q += boost::lexical_cast<std::string>(abs(zmin - zmax));
	q += "],[";
	q += boost::lexical_cast<std::string>(xmid);
	q += ",";
	q += boost::lexical_cast<std::string>(ymid);
	q += ",";
	q += boost::lexical_cast<std::string>(zmid);
	q += "],";
	q += boost::lexical_cast<std::string>(particles);
	q += ")";

	//cout << q << endl;
	sendQuery(q);
}

string clusterObsBBIteration(
		tabletop_object_detector::TabletopDetection detection_call, int i,
		int npoints) {
	float xmin = std::numeric_limits<float>::max();
	float xmax = std::numeric_limits<float>::min();
	float ymin = std::numeric_limits<float>::max();
	float ymax = std::numeric_limits<float>::min();
	float zmin = std::numeric_limits<float>::max();
	float zmax = std::numeric_limits<float>::min();

	for (int j = 0; j < npoints; j++) {
		if (detection_call.response.detection.clusters[i].points[j].x < xmin) {
			xmin = detection_call.response.detection.clusters[i].points[j].x;
		}
		if (detection_call.response.detection.clusters[i].points[j].x > xmax) {
			xmax = detection_call.response.detection.clusters[i].points[j].x;
		}
		if (detection_call.response.detection.clusters[i].points[j].y < ymin) {
			ymin = detection_call.response.detection.clusters[i].points[j].y;
		}
		if (detection_call.response.detection.clusters[i].points[j].y > ymax) {
			ymax = detection_call.response.detection.clusters[i].points[j].y;
		}
		if (detection_call.response.detection.clusters[i].points[j].z < zmin) {
			zmin = detection_call.response.detection.clusters[i].points[j].z;
		}
		if (detection_call.response.detection.clusters[i].points[j].z > zmax) {
			zmax = detection_call.response.detection.clusters[i].points[j].z;
		}
	}

	float xmid = (xmin + xmax) / 2;
	float ymid = (ymin + ymax) / 2;
	float zmid = (zmin + zmax) / 2;

	string q = boost::lexical_cast<std::string>(xmid);
	q += ",";
	q += boost::lexical_cast<std::string>(ymid);
	q += ",";
	q += boost::lexical_cast<std::string>(zmid);
	q += ",";
	q += boost::lexical_cast<std::string>(abs(xmin - xmax));
	q += ",";
	q += boost::lexical_cast<std::string>(abs(ymin - ymax));
	q += ",";
	q += boost::lexical_cast<std::string>(abs(zmin - zmax));
	return q;
}

std::vector<int> getRGB(float rgb_encoded) //, unsigned char &r, unsigned char &g, unsigned char &b)
		{

	union {
		float input; // assumes sizeof(float) == sizeof(int)
		int output;
	} data;

	data.input = rgb_encoded;
	std::bitset<sizeof(float) * CHAR_BIT> bits(data.output);

	std::bitset<sizeof(8) * CHAR_BIT> red;
	for (int i = 0; i < 8; i++) {
		red[i] = bits[16 + i];
	}
	string redstring = red.to_string();
	int rvalue = toInt(redstring.c_str(), 32);
	//cout << rvalue << endl;

	std::bitset<sizeof(8) * CHAR_BIT> green;
	for (int i = 0; i < 8; i++) {
		green[i] = bits[8 + i];
	}
	string greenstring = green.to_string();
	int gvalue = toInt(greenstring.c_str(), 32);
	//cout << gvalue << endl;

	std::bitset<sizeof(8) * CHAR_BIT> blue;
	for (int i = 0; i < 8; i++) {
		blue[i] = bits[i];
	}
	string bluestring = blue.to_string();
	int bvalue = toInt(bluestring.c_str(), 32);
	//cout << bvalue << endl;

	std::vector<int> rgb(3);
	rgb[0] = rvalue;
	rgb[1] = gvalue;
	rgb[2] = bvalue;
	return rgb;
}
void objectsObservationsIteration(
		tabletop_object_detector::TabletopDetection detection_call,
		ros::NodeHandle nh) {
	while (!ros::service::waitForService(
			"/objects_database_node/get_model_description", ros::Duration(2.0))
			&& nh.ok()) {
		ROS_INFO("Waiting for service to come up");
	}

	int nclusters = detection_call.response.detection.clusters.size();
	string q = "objectsObservationIterationParticles(";
	q += boost::lexical_cast<std::string>(nclusters);
	q += ",[";

	for (int i = 0; i < nclusters; i++) {
		if(i>0){
			q += ",";
		}
		q += "[";
		int nchannels =
				detection_call.response.detection.clusters[i].channels.size();
		int npoints =
				detection_call.response.detection.clusters[i].points.size();
		string loc = clusterObsBBIteration(detection_call, i, npoints);
		q += loc;
		int red = 0;
		int green = 0;
		int blue = 0;

		int nvalues =
				detection_call.response.detection.clusters[i].channels[0].values.size();
		for (int j = 0; j < nvalues; j++) {
			float f =
					detection_call.response.detection.clusters[i].channels[0].values[j];
			std::vector<int> rgb = getRGB(f);
			red += rgb[0];
			green += rgb[1]; //TODO fout?
			blue += rgb[2];
		}
		double redglobal = red / nvalues;
		double greenglobal = green / nvalues;
		double blueglobal = blue / nvalues;
		q += ",";
		q += boost::lexical_cast<std::string>(redglobal);
		q += ",";
		q += boost::lexical_cast<std::string>(greenglobal);
		q += ",";
		q += boost::lexical_cast<std::string>(blueglobal);

		q += ",[";
		int nmodels2 =
				detection_call.response.detection.models[i].model_list.size();
		for (int w = 0; w < nmodels2; w++) {
			if (w > 0) {
			}
			ros::ServiceClient househ_srv = nh.serviceClient<
					household_objects_database_msgs::GetModelDescription>(
					"/objects_database_node/get_model_description", true);
			household_objects_database_msgs::GetModelDescription model_desc;
			model_desc.request.model_id =
					detection_call.response.detection.models[i].model_list[w].model_id;
			househ_srv.call(model_desc);


			int ntags = model_desc.response.tags.size();
			if (w != 0) {
				q += ",";
			}
			q +=
					boost::lexical_cast<std::string>(
							1
									/ detection_call.response.detection.models[i].model_list[w].confidence);
			q += ":";
			//cout << model_desc.request.model_id << endl;
						q += boost::lexical_cast<std::string>(model_desc.request.model_id);
			for (int s = 0; s < 1; s++) { //alle tags
				if (s != 0) {
					q += ",";
				}
				//q += model_desc.response.tags[s];
			}
		}
		q += "]]";
	}
	q += "],";
	q += boost::lexical_cast<std::string>(particles);
	q += ")";

	//cout << q << endl;
	sendQuery(q);//TODO TODO
}

tabletop_collision_map_processing::TabletopCollisionMapProcessing scanTableIteration(
		ros::ServiceClient object_detection_srv,
		ros::ServiceClient collision_processing_srv, ros::NodeHandle nh) {
	ROS_INFO("Scanning table iteration.");
	tabletop_object_detector::TabletopDetection detection_call;
	detection_call.request.return_clusters = true;
	detection_call.request.return_models = true;
	detection_call.request.num_models = 100;

	if (!object_detection_srv.call(detection_call)) {
		ROS_ERROR("Scanning table for first time failed.");
	}
	if (detection_call.response.detection.result
			!= detection_call.response.detection.SUCCESS) {
		ROS_ERROR(
				"Scanning table for first time failed: Tabletop detection returned error code %d", detection_call.response.detection.result);
	}
	if (detection_call.response.detection.clusters.empty()
			&& detection_call.response.detection.models.empty()) {
		//ROS_ERROR(
		//		"Scanning table for first time failed: The tabletop detector detected the table, " "but found no objects");
	}

	//objectsObservationsIteration(detection_call, nh);
	//TODO
	tabletop_collision_map_processing::TabletopCollisionMapProcessing processing_call;
	processing_call.request.detection_result =
			detection_call.response.detection;
	processing_call.request.reset_collision_models = true;
	processing_call.request.reset_attached_models = true;
	processing_call.request.desired_frame = "base_link";

	if (!collision_processing_srv.call(processing_call)) {
		ROS_ERROR("Collision map processing service failed");
	}
	if (processing_call.response.graspable_objects.empty()) {
		ROS_INFO("No objects to grasp detected");
		//ROS_ERROR("Collision map processing returned no graspable objects");
	}
	return processing_call;
}

#include <iostream>
#include <bitset>

void objectsObservationsInit(
		tabletop_object_detector::TabletopDetection detection_call,
		ros::NodeHandle nh) {
	while (!ros::service::waitForService(
			"/objects_database_node/get_model_description", ros::Duration(2.0))
			&& nh.ok()) {
		ROS_INFO("Waiting for service to come up");
	}

	int nclusters = detection_call.response.detection.clusters.size();
	string q = "nobjsObservationInit(";
	q += boost::lexical_cast<std::string>(nclusters);
	q += ",";
	q += boost::lexical_cast<std::string>(particles);
	q += ")";

	//cout << q << endl;
	sendQuery(q);

	ros::ServiceClient househ_srv = nh.serviceClient<
						household_objects_database_msgs::GetModelDescription>(
						"/objects_database_node/get_model_description", true);

	for (int i = 0; i < nclusters; i++) {
		int nchannels =
				detection_call.response.detection.clusters[i].channels.size();
		int red = 0;
		int green = 0;
		int blue = 0;

		int nvalues =
				detection_call.response.detection.clusters[i].channels[0].values.size();
		for (int j = 0; j < nvalues; j++) {
			float f =
					detection_call.response.detection.clusters[i].channels[0].values[j];
			std::vector<int> rgb = getRGB(f);
			red += rgb[0];
			green += rgb[1];
			blue += rgb[2];
		}

		/*
		cout << red << endl;
		cout << green << endl;
		cout << blue << endl;
		*/

		double redglobal = red / nvalues;
		double greenglobal = green / nvalues;
		double blueglobal = blue / nvalues;

		string qc = "colorObservationInit(";
		qc += boost::lexical_cast<std::string>(i + 1);
		qc += ",[";
		qc += boost::lexical_cast<std::string>(redglobal);
		qc += ",";
		qc += boost::lexical_cast<std::string>(greenglobal);
		qc += ",";
		qc += boost::lexical_cast<std::string>(blueglobal);
		qc += "],";
		qc += boost::lexical_cast<std::string>(particles);
		qc += ")";
		//cout << endl;
		//cout << qc << endl;
		sendQuery(qc);

		int npoints =
				detection_call.response.detection.clusters[i].points.size();
		clusterObsBBInit(detection_call, i, npoints);
	}

	int nmodels = detection_call.response.detection.models.size();
	//cout << nmodels << endl;
	int nmodels2;
	for (int l = 0; l < nmodels; l++) {
		nmodels2=
						detection_call.response.detection.models[l].model_list.size();
				//cout << nmodels2 << endl;
	}

	for (int l = 0; l < nmodels; l++) {
		string q2 = "typeObservationInit(";
		q2 += boost::lexical_cast<std::string>(l + 1);
		q2 += ",[";

		int nmodels2 =
				detection_call.response.detection.models[l].model_list.size();
		//cout << nmodels2 << endl;
		for (int w = 0; w < nmodels2; w++) {

			household_objects_database_msgs::GetModelDescription model_desc;
			model_desc.request.model_id =
					detection_call.response.detection.models[l].model_list[w].model_id;
			househ_srv.call(model_desc);
			//cout << model_desc.request.model_id << endl;


			int ntags = model_desc.response.tags.size();
			if (w != 0) {
				q2 += ",";
			}

			q2 +=
					boost::lexical_cast<std::string>(
							1
									/ detection_call.response.detection.models[l].model_list[w].confidence);
			/*
			cout
					<< detection_call.response.detection.models[l].model_list[w].confidence
					<< endl;
			*/
			q2 += ":";
			q2 += boost::lexical_cast<std::string>(model_desc.request.model_id);
			for (int s = 0; s < 1; s++) { //alle tags
				if (s != 0) {
					q2 += ",";
				}
				//q2 += model_desc.response.tags[s];
				//cout << model_desc.response.tags[s] << endl;
			}
		}
		q2 += "],";
		q2 += boost::lexical_cast<std::string>(particles);
		q2 += ")";
		//cout << q2 << endl;
		sendQuery(q2);
	}
}

void tableObservationsInit(tabletop_object_detector::TabletopDetection det) {
	tf::TransformListener listener;
	geometry_msgs::PoseStamped pose = det.response.detection.table.pose;

	btScalar x = pose.pose.orientation.x;
	btScalar y = pose.pose.orientation.y;
	btScalar z = pose.pose.orientation.z;
	btScalar w = pose.pose.orientation.w;

	btScalar x2 = pose.pose.position.x;
	btScalar y2 = pose.pose.position.y;
	btScalar z2 = pose.pose.position.z;

	btTransform t1;
	t1.setOrigin(btVector3(x2, y2, z2));
	t1.setRotation(btQuaternion(x, y, z, w));
	btTransform t2 = t1.inverse();

	geometry_msgs::PoseStamped pose2;
	pose2.pose.position.x = t2.getOrigin().getX();
	pose2.pose.position.y = t2.getOrigin().getY();
	pose2.pose.position.z = t2.getOrigin().getZ();
	pose2.pose.orientation.x = t2.getRotation().getX();
	pose2.pose.orientation.y = t2.getRotation().getY();
	pose2.pose.orientation.z = t2.getRotation().getZ();
	pose2.pose.orientation.w = t2.getRotation().getW();

	btVector3 xmin1;
	xmin1.setX(det.response.detection.table.x_min);
	xmin1.setY(0);
	xmin1.setZ(0);
	btVector3 xmin2 = t1 * xmin1;

	btVector3 xmax1;
	xmax1.setX(det.response.detection.table.x_max);
	xmax1.setY(0);
	xmax1.setZ(0);
	btVector3 xmax2 = t1 * xmax1;

	btVector3 ymin1;
	ymin1.setX(0);
	ymin1.setY(det.response.detection.table.y_min);
	ymin1.setZ(0);
	btVector3 ymin2 = t1 * ymin1;

	btVector3 ymax1;
	ymax1.setX(0);
	ymax1.setY(det.response.detection.table.y_max);
	ymax1.setZ(0);
	btVector3 ymax2 = t1 * ymax1;

	btVector3 z1;
	z1.setX(0);
	z1.setY(0);
	z1.setZ(0);
	btVector3 ztransf = t1 * z1;

	float xmid = (xmin2.getX() + xmax2.getX()) / 2;
	float ymid = (ymin2.getY() + ymax2.getY()) / 2;
	float zmid = ((ztransf.getZ() - 0.2) + ztransf.getZ()) / 2;

	string q = "tableObservation([";
	q += boost::lexical_cast<std::string>(abs(xmin2.getX() - xmax2.getX()));
	q += ",";
	q += boost::lexical_cast<std::string>(abs(ymin2.getY() - ymax2.getY()));
	q += ",";
	q += boost::lexical_cast<std::string>(
			abs((ztransf.getZ() - 0.2) - ztransf.getZ()));
	q += "],[";
	q += boost::lexical_cast<std::string>(xmid);
	q += ",";
	q += boost::lexical_cast<std::string>(ymid);
	q += ",";
	q += boost::lexical_cast<std::string>(zmid);
	q += "],";
	q += boost::lexical_cast<std::string>(particles);
	q += ")";
	//cout << q << endl;
	sendQuery(q);
}

void scanTableInit(ros::ServiceClient object_detection_srv,
		ros::ServiceClient collision_processing_srv, ros::NodeHandle nh) {
	ROS_INFO("Scanning table for first time.");
	tabletop_object_detector::TabletopDetection detection_call;
	detection_call.request.return_clusters = true;//TODO
	detection_call.request.return_models = true;
	detection_call.request.num_models = 16;

	if (!object_detection_srv.call(detection_call)) {
		ROS_ERROR("Scanning table for first time failed.");
	}
	if (detection_call.response.detection.result
			!= detection_call.response.detection.SUCCESS) {
		ROS_ERROR(
				"Scanning table for first time failed: Tabletop detection returned error code %d", detection_call.response.detection.result);
	}
	if (detection_call.response.detection.clusters.empty()
			&& detection_call.response.detection.models.empty()) {
		ROS_ERROR(
				"Scanning table for first time failed: The tabletop detector detected the table, " "but found no objects");
	}

	int nmodels = detection_call.response.detection.models.size();
		cout << nmodels << endl;
		int nmodels2;
		for (int l = 0; l < nmodels; l++) {
			nmodels2=
							detection_call.response.detection.models[l].model_list.size();
					cout << nmodels2 << endl;
		}

	tableObservationsInit(detection_call);
	objectsObservationsInit(detection_call, nh);

	tabletop_collision_map_processing::TabletopCollisionMapProcessing processing_call;
	processing_call.request.detection_result =
			detection_call.response.detection;
	processing_call.request.reset_collision_models = true;
	processing_call.request.reset_attached_models = true;
	processing_call.request.desired_frame = "base_link";

	if (!collision_processing_srv.call(processing_call)) {
		ROS_ERROR("Collision map processing service failed");
	}
	if (processing_call.response.graspable_objects.empty()) {
		ROS_ERROR("Collision map processing returned no graspable objects");
	}
}

void dcpf(int particles) {

	YAP_Term error;
	int res = YAP_RunGoal(YAP_ReadBuffer("use_module('init.pl')", &error));
	if (res == 1)
		ROS_INFO("OK: module loaded");
	else
		ROS_INFO("Error: module not loaded");

	std::string q = "initdcpf(";
	q += boost::lexical_cast<std::string>(particles);
	q += ")";
	sendQuery(q);
}

int comm_n = 1;
void callback(const std_msgs::StringConstPtr& str) {
	cout << str.get() << endl;
}

#include <vector>

std::vector<double> getMidPoint(sensor_msgs::PointCloud points) {

	float xmin = std::numeric_limits<float>::max();
	float xmax = std::numeric_limits<float>::min();
	float ymin = std::numeric_limits<float>::max();
	float ymax = std::numeric_limits<float>::min();
	float zmin = std::numeric_limits<float>::max();
	float zmax = std::numeric_limits<float>::min();

	int npoints = points.points.size();
	for (int j = 0; j < npoints; j++) {
		if (points.points[j].x < xmin) {
			xmin = points.points[j].x;
		}
		if (points.points[j].x > xmax) {
			xmax = points.points[j].x;
		}
		if (points.points[j].y < ymin) {
			ymin = points.points[j].y;
		}
		if (points.points[j].y > ymax) {
			ymax = points.points[j].y;
		}
		if (points.points[j].z < zmin) {
			zmin = points.points[j].z;
		}
		if (points.points[j].z > zmax) {
			zmax = points.points[j].z;
		}
	}

	double xmid = (xmin + xmax) / 2;
	double ymid = (ymin + ymax) / 2;
	double zmid = (zmin + zmax) / 2;
	std::vector<double> mid(3);

	mid[0] = xmid;
	mid[1] = ymid;
	mid[2] = zmid;
	return mid;
}

int getMinIndex(double array[]) {
	double min = 100000;
	int index = -1;
	int size = ARRAY_SIZE(array);
	for (int j = 0; j < size; j++) {
		if (array[j] < min) {
			min = array[j];
			index = j;
		}
	}
	return index;
}

int getGraspableObject(double x, double y, double z,
		tabletop_collision_map_processing::TabletopCollisionMapProcessing processing_call) {
	int n = processing_call.response.graspable_objects.size();
	double distances[n];
	for (int i = 0; i < n; i++) {
		sensor_msgs::PointCloud points =
				processing_call.response.graspable_objects.at(i).cluster;
		std::vector<double> p = getMidPoint(points);
		distances[i] = sqrt(
				pow(x - p[0], 2.0) + pow(y - p[1], 2.0) + pow(z - p[2], 2.0));
		cout << i << " distance:" << distances[i] << endl;
	}
	int index = getMinIndex(distances);
	return index;
}

#include <sstream>
double string_to_double(const std::string& s) {
	std::istringstream i(s);
	double x;
	if (!(i >> x))
		return 0;
	return x;
}

bool perform_action(string action, string param[],
		tabletop_collision_map_processing::TabletopCollisionMapProcessing processing_call,
		ros::NodeHandle nh, LeftArm& left, RightArm& right) {
	bool success = false;

	if (action == "none") {
		cout << "Nothing to execute" << endl;
	}

	if (action == "pickup") {
		cout << param[1] << endl;
		double x = string_to_double(param[1]);
		double y = string_to_double(param[2]);
		double z = string_to_double(param[3]);
		cout << z << endl;
		int index_go = getGraspableObject(x, y, z, processing_call);
		cout << index_go << endl;
		if (param[4] == "right_gripper") {
			right.pickup(processing_call, index_go, nh);
			geometry_msgs::PoseStamped start_pose2 = right.getPosition(nh,
					"base_link");
			right.setPickupLocation(start_pose2);
			right.setOrientation(start_pose2);
			//right.moveSideStandard(nh);

			right.moveSide(nh, start_pose2.pose.orientation.x,
					start_pose2.pose.orientation.y,
					start_pose2.pose.orientation.z,
					start_pose2.pose.orientation.w);

		}
		if (param[4] == "left_gripper") {
			left.pickup(processing_call, index_go, nh);
			geometry_msgs::PoseStamped start_pose2 = left.getPosition(nh,
					"base_link");
			left.setPickupLocation(start_pose2);
			left.setOrientation(start_pose2);
			//left.moveSideStandard(nh);

			left.moveSide(nh, start_pose2.pose.orientation.x,
					start_pose2.pose.orientation.y,
					start_pose2.pose.orientation.z,
					start_pose2.pose.orientation.w);


		}
	}
	if (action == "place") {
		if (param[4] == "right_gripper") {
			bool place = false;
			int i = 0;
			while (!place) {
				double x = right.pickup_location.getX();
				double y = right.pickup_location.getY();
				cout << right.pickup_location.getZ() << endl;
				double z = right.pickup_location.getZ() - 0.3 + i * 0.001;//
				//EXTENSIE redeneren waar plaats op tafel: andere loc krijgen van dcpf
				cout << "Trying place for z coord: " << z << endl;
				place = right.moveToLocation(nh, x, y, z);
				i++;
			}
			RightGripper rgrip;
			rgrip.open();
			ros::Duration(2).sleep();
			right.moveSideStandard(nh);
		}
		if (param[4] == "left_gripper") {
			bool place = false;
			int i = 0;
			while (!place) {
				double x = left.pickup_location.getX();
				double y = left.pickup_location.getY();
				double z = left.pickup_location.getZ() - 0.3 + i * 0.001;//- 0.3
				//EXTENSIE redeneren waar plaats op tafel: andere loc krijgen van dcpf
				cout << "Trying place for z coord: " << z << endl;
				place = left.moveToLocation(nh, x, y, z);
				i++;
			}
			LeftGripper lgrip;
			lgrip.open();
			ros::Duration(2).sleep();
			left.moveSideStandard(nh);
		}
	}
	if (action == "info") {
		string qinfo = "obj_current_query(";
		qinfo += param[0];
		qinfo += ",";
		qinfo += boost::lexical_cast<std::string>(particles);
		qinfo += ")";
		//cout << qinfo << endl;
		sendQuery(qinfo);

		string qwa = "current_wa_query(";
		qwa += "human(1)"; //TODO
		qwa += ",";
		qwa += boost::lexical_cast<std::string>(particles);
		qwa += ")";
		//cout << qwa << endl;
		sendQuery(qwa);
	}
	success = true; //TODO
	return success;
}
int main(int argc, char **argv) {
	ros::init(argc, argv, "get_ik");
	ros::NodeHandle nh;

	/*
	geometry_msgs::PoseStamped pose;

			ros::ServiceClient get_state_client_ = nh.serviceClient<
					arm_navigation_msgs::GetRobotState>(
					"/environment_server/get_robot_state");
			arm_navigation_msgs::GetRobotState::Request req;
			arm_navigation_msgs::GetRobotState::Response res;
			get_state_client_.call(req, res);
			std::cout << res.error_code << std::endl;

			ros::service::waitForService("pr2_left_arm_kinematics/get_fk");
			ros::ServiceClient fk_client = nh.serviceClient<
					kinematics_msgs::GetPositionFK>(
					"pr2_left_arm_kinematics/get_fk");

			kinematics_msgs::GetPositionFK::Request fk_request;
			kinematics_msgs::GetPositionFK::Response fk_response;

			fk_request.header.frame_id = "base_link";
			fk_request.fk_link_names.resize(1);
			fk_request.fk_link_names[0] = "l_wrist_roll_link";
			fk_request.robot_state = res.robot_state;
			if (fk_client.call(fk_request, fk_response)) {
				std::cout << fk_response.fk_link_names[0] << std::endl;
				std::cout << fk_response.pose_stamped[0].header.frame_id
						<< std::endl;
				pose = fk_response.pose_stamped[0];
				std::cout << "Wrist pose currently:" << std::endl;
						std::cout << pose.pose.position.x << std::endl;
						std::cout << pose.pose.position.y << std::endl;
						std::cout << pose.pose.position.z << std::endl;
						std::cout << pose.pose.orientation.x << std::endl;
						std::cout << pose.pose.orientation.y << std::endl;
						std::cout << pose.pose.orientation.z << std::endl;
						std::cout << pose.pose.orientation.w << std::endl;

			} else {
				//std::cout << "can't get link info" << std::endl;
			}
*/

	int n_particles = 1000;

	if (YAP_FastInit(NULL) == YAP_BOOT_ERROR)
		return 1;

	dcpf(n_particles);
	particles = n_particles;
	ros::ServiceClient object_detection_srv;
	ros::ServiceClient collision_processing_srv;

	while (!ros::service::waitForService("/object_detection",
			ros::Duration(10.0)) && nh.ok()) {
		ROS_INFO("Waiting for object detection service to come up");
	}
	if (!nh.ok())
		exit(0);
	object_detection_srv = nh.serviceClient<
			tabletop_object_detector::TabletopDetection>("/object_detection",
			true);

	while (!ros::service::waitForService(
			"/tabletop_collision_map_processing/tabletop_collision_map_processing",
			ros::Duration(10.0)) && nh.ok()) {
		ROS_INFO("Waiting for collision processing service to come up");
	}
	if (!nh.ok())
		exit(0);
	collision_processing_srv =
			nh.serviceClient<
					tabletop_collision_map_processing::TabletopCollisionMapProcessing>(
					"/tabletop_collision_map_processing/tabletop_collision_map_processing",
					true);

	LeftArm left;
	RightArm right;
	left.moveSide(nh);
	right.moveSide(nh);

	RobotHead head;
	head.lookdown();

	collision_processing_srv =
			nh.serviceClient<
					tabletop_collision_map_processing::TabletopCollisionMapProcessing>(
					"/tabletop_collision_map_processing/tabletop_collision_map_processing",
					true);

	scanTableInit(object_detection_srv, collision_processing_srv, nh);
	//tabletop_object_detector::TabletopDetection detection_call = head.scanTable(nh,collision_processing_srv,object_detection_srv);

	bool end = false;
	while (end == false) {
		string input;
		cout << endl;
		cout << "What do you want to do? scan/action/dcpf/end" << endl;
		cout << "scan: scan table once" << endl;
		cout << "action: allows to define your action need" << endl;
		cout << "dcpf: query knowledge base" << endl;
		cout << "end: exit program" << endl;
		cout << endl;

		cin >> input;
		if (input == "scan") {
			cout << "Scanning table iteration started." << endl;
			scanTableIteration(object_detection_srv, collision_processing_srv,
					nh);
		} else if (input == "pickup") {
			geometry_msgs::PoseStamped start_pose = left.getPosition(nh,
					"base_link");
			left.setPickupLocation(start_pose);
			left.setOrientation(start_pose);
			left.moveSide(nh, 0, 0, 0, 1);
		} else if (input == "place") {
			double x = left.pickup_location.getX();
			double y = left.pickup_location.getY();
			double z = left.pickup_location.getZ() - 0.30;
			left.moveToLocation(nh, x, y, z);
		} else if (input == "action") {

			tabletop_collision_map_processing::TabletopCollisionMapProcessing processing_call =
					scanTableIteration(object_detection_srv,
							collision_processing_srv, nh);

			string qn = "needBeginObservation(human(1),";
			qn += boost::lexical_cast<std::string>(particles);
			qn += ")";
			//cout << qn << endl;
			sendQuery(qn);

			string q3 = "actionDemandObservation(human(1),";
			q3 += boost::lexical_cast<std::string>(particles);
			q3 += ")";
			//cout << q3 << endl;
			sendQuery(q3);

			bool satisfied_user = false;

			while (!satisfied_user) {
				string input2;
				cout << "Give statement:" << endl;
				cin >> input2;

				string q4 = "commandObservation(human(1),";
				q4 += input2;
				q4 += ",";
				q4 += boost::lexical_cast<std::string>(particles);
				q4 += ")";
				//cout << q4 << endl;
				sendQuery(q4);

				cout << "Calculating best action." <<endl;
				YAP_Term arg[2];
				//arg[0] = YAP_Term("human(1)");
				arg[0] = YAP_MkVarTerm();
				//arg[2] = YAP_Term("1000");
				arg[1] = YAP_MkIntTerm(particles);

				YAP_Term tmp = YAP_MkApplTerm(
						YAP_MkFunctor(YAP_LookupAtom("get_best_waction"), 2), 2,
						arg);
				long safe_t = YAP_InitSlot(tmp);

				int res = YAP_RunGoal(tmp);
				char s[100];
				YAP_WriteBuffer(YAP_ArgOfTerm(1, YAP_GetFromSlot(safe_t)), s,
						100, YAP_WRITE_HANDLE_VARS);

				YAP_RecoverSlots(1);
				string result = s;
				cout << result << endl;
				string result2 = "(";
				result2 += result;
				result2 += ")";
				cout << result2 << endl;

				std::stringstream test(result2);
				std::string segment;
				std::vector<std::string> seglist;
				while (std::getline(test, segment, ',')) {
					seglist.push_back(segment);
				}

				std::stringstream test1(seglist.at(0));
				std::string segment1;
				std::vector<std::string> seglist1;
				while (std::getline(test1, segment1, '(')) {
					seglist1.push_back(segment1);
				}

				std::stringstream test3(seglist.at(2));
				std::string segment3;
				std::vector<std::string> seglist3;
				while (std::getline(test3, segment3, '[')) {
					seglist3.push_back(segment3);
				}

				std::stringstream test7(seglist.at(6));
				std::string segment7;
				std::vector<std::string> seglist7;
				while (std::getline(test7, segment7, ']')) {
					seglist7.push_back(segment7);
				}

				string performance_name = seglist1.at(1);
				string action = seglist.at(1);

				string param[5];
				param[0] = seglist3.at(1);
				param[1] = seglist.at(3);
				param[2] = seglist.at(4);
				param[3] = seglist.at(5);
				param[4] = seglist7.at(0);

				string qstart = "start_performance(";
				qstart += performance_name;
				qstart += ",";
				qstart += boost::lexical_cast<std::string>(particles);
				qstart += ")";
				//cout << qstart << endl;
				sendQuery(qstart);

				//cout << action << endl;

				bool end = perform_action(action, param, processing_call,
						nh, left, right);
				if (end) {
					string qsuccess = "end_performance(";
					qsuccess += performance_name;
					qsuccess += ",";
					qsuccess += boost::lexical_cast<std::string>(particles);
					qsuccess += ")";
					//cout << qsuccess << endl;
					sendQuery(qsuccess);

					//cout << action << endl;
					if(action == "pickup" || action == "place"){

					string feedbackeffect;
					cout << "Did the action have the right effect? (yes or no)" << endl;
					cin >> feedbackeffect;

					if(feedbackeffect == "yes"){
												string qeff = "pos_wanted_effect(";
												qeff += performance_name;
												qeff += ",";
												qeff += boost::lexical_cast<std::string>(particles);
												qeff += ")";
												//cout << qeff << endl;
												sendQuery(qeff);
					}
					else{
						string qeff = "neg_wanted_effect(";
																		qeff += performance_name;
																		qeff += ",";
																		qeff += boost::lexical_cast<std::string>(particles);
																		qeff += ")";
																		//cout << qeff << endl;
																		sendQuery(qeff);
					}
					}
					string feedback;
					cout << "Was this what you wanted? (yes or no)" << endl;
					cin >> feedback;
					//tabletop_collision_map_processing::TabletopCollisionMapProcessing processing_call2 = scanTableIteration(object_detection_srv,collision_processing_srv,nh);
					//TODO human ook doorgeven
					if (feedback == "yes") {
						satisfied_user = true;
						string q5 = "positive_feedback(";
						q5 += boost::lexical_cast<std::string>(
								performance_name);
						q5 += ",";
						q5 += boost::lexical_cast<std::string>(particles);
						q5 += ")";
						//cout << q5 << endl;
						sendQuery(q5);
					}
					if (feedback == "no") {

						YAP_Term arg3[2];
						arg3[0] = YAP_MkIntTerm(particles);
						arg3[1] = YAP_MkVarTerm();

						YAP_Term tmp3 = YAP_MkApplTerm(
								YAP_MkFunctor(
										YAP_LookupAtom("negative_feedback"), 2),
								2, arg3);
						long safe_t3 = YAP_InitSlot(tmp3);

						int res3 = YAP_RunGoal(tmp3);
						char s3[100];
						YAP_WriteBuffer(
								YAP_ArgOfTerm(2, YAP_GetFromSlot(safe_t3)), s3,
								100, YAP_WRITE_HANDLE_VARS);
						YAP_RecoverSlots(1);
						string result3 = s3;
						string result2a = "(";
						result2a += result3;
						result2a += ")";
						//cout << result2a << endl;

						std::stringstream testa(result2a);
						std::string segmenta;
						std::vector<std::string> seglista;
						while (std::getline(testa, segmenta, ',')) {
							seglista.push_back(segmenta);
						}

						std::stringstream test1a(seglista.at(0));
						std::string segment1a;
						std::vector<std::string> seglist1a;
						while (std::getline(test1a, segment1a, '(')) {
							seglist1a.push_back(segment1a);
						}
						std::stringstream test3a(seglista.at(2));
						std::string segment3a;
						std::vector<std::string> seglist3a;
						while (std::getline(test3a, segment3a, '[')) {
							seglist3a.push_back(segment3a);
						}
						std::stringstream test7a(seglista.at(6));
						std::string segment7a;
						std::vector<std::string> seglist7a;
						while (std::getline(test7a, segment7a, ']')) {
							seglist7a.push_back(segment7a);
						}

						string performance_namea = seglist1a.at(1);
						string actiona = seglista.at(1);
						string parama[5];
						parama[0] = seglist3a.at(1);
						parama[1] = seglista.at(3);
						parama[2] = seglista.at(4);
						parama[3] = seglista.at(5);
						parama[4] = seglist7a.at(0);

						satisfied_user = false;
						string qstart2 = "start_performance(";
						qstart2 += performance_namea;
						qstart2 += ",";
						qstart2 += boost::lexical_cast<std::string>(particles);
						qstart2 += ")";
						cout << "compensation action unwanted" << endl;
						//cout << qstart2 << endl;
						sendQuery(qstart2);
						/*TODO
						string qs = "positive_feedback(";
						qs += boost::lexical_cast<std::string>(
								performance_namea);
						qs += ",";
						qs += boost::lexical_cast<std::string>(particles);
						qs += ")";
						cout << qs << endl;
						sendQuery(qs);
						*/
						cout << "compensation action performactionproc" << endl;
						bool successa = perform_action(actiona, parama,
								processing_call, nh, left, right);
						if (successa) {
							string qsuccessa = "end_performance(";
							qsuccessa += performance_namea;
							qsuccessa += ",";
							qsuccessa += boost::lexical_cast<std::string>(
									particles);
							qsuccessa += ")";
							//cout << qsuccessa << endl;
							sendQuery(qsuccessa);
						} else {
							string q_unsuccessa = "unsuccess_performance(";
							q_unsuccessa += performance_namea;
							q_unsuccessa += ",";
							q_unsuccessa += boost::lexical_cast<std::string>(
									particles);
							q_unsuccessa += ")";
							//cout << q_unsuccessa << endl;
							sendQuery(q_unsuccessa);
							//cout << "Could not perform wanted action" << endl;
						}

					}


				} else {
					//cout << "unsucc" << endl;
					string q_unsuccess = "unsuccess_performance(";
					q_unsuccess += performance_name;
					q_unsuccess += ",";
					q_unsuccess += boost::lexical_cast<std::string>(particles);
					q_unsuccess += ")";
					//cout << q_unsuccess << endl;
					sendQuery(q_unsuccess);
					//cout << "Could not perform wanted action" << endl; //EXTENSIE naar dcpf juiste update
				}
			}

			//cout << args[2] << endl;
			/*
			 out = YAP_RunGoal(t);
			 t = YAP_GetFromSlot(sl);
			 YAP_RecoverSlots(1);
			 if (out == 0) return FALSE;

			 string q5 = "get_best_waction(H,T,N)"
			 q5 += "human(1),";
			 q5
			 */
		} else if (input == "dcpf") {
			string input3;
			cout << "Give dcpf query:" << endl;
			cin >> input3;
			sendQuery(input3);
		} else if (input == "leftside") {
			left.moveSideStandard(nh);
		} else if (input == "leftside2") {
			geometry_msgs::PoseStamped start_pose = left.getPosition(nh,
					"base_link");
			double magnitude = sqrt(
					pow(start_pose.pose.orientation.x, 2.0)
							+ pow(start_pose.pose.orientation.y, 2.0)
							+ pow(start_pose.pose.orientation.z, 2.0)
							+ pow(start_pose.pose.orientation.w, 2.0));
			double w = start_pose.pose.orientation.w / magnitude;
			double x = start_pose.pose.orientation.x / magnitude;
			double y = start_pose.pose.orientation.y / magnitude;
			double z = start_pose.pose.orientation.z / magnitude;
			left.moveSide(nh, x, y, z, w);
		}

		else if (input == "rightside") {
			right.moveSide(nh);
		}

		else if (input == "end") {
			cout << "Closing program" << endl;
			end = true;
		} else {
			cout << "Command does not exist"<< endl;
		}
	}
	return 0;
}

//get complex joint information
/*
		 int njoints = res.robot_state.joint_state.name.size();
		 std::cout << "Number of joints found: " << njoints;
		 for (int i = 0 ; i < njoints ; i++) {
		 //	std::cout << i;
		 //	std::cout << res.robot_state.joint_state.name[i] << std::endl;
		 }
		 */

		//std::cout << res.robot_state.joint_state.header;
		//std::cout << res.robot_state.joint_state.name[43] << std::endl;
		//std::cout << res.robot_state.joint_state.effort[43] << std::endl;
		//std::cout << res.robot_state.joint_state.position[43] << std::endl;
		//std::cout << res.robot_state.joint_state.velocity[43] << std::endl;
		//std::cout << res.robot_state.joint_state.header.frame_id << std::endl ;
		//std::cout << res.robot_state.joint_state.name[28] << std::endl;
		//std::cout << res.robot_state.joint_state.effort[28] << std::endl;
		//std::cout << res.robot_state.joint_state.position[28] << std::endl;
		//std::cout << res.robot_state.joint_state.velocity[28] << std::endl;
