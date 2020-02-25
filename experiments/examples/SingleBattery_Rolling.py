#!/usr/bin/env python3
"""
This script loads a robot.yaml file and inserts it into the simulator.
"""

import os
import sys
import asyncio
from pyrevolve.SDF.math import Vector3
from pyrevolve import revolve_bot, parser
from pyrevolve.tol.manage import World
from pyrevolve.util.supervisor.supervisor_multi import DynamicSimSupervisor
from pyrevolve.evolution import fitness
from pyrevolve.tol.manage import measures as Measures
import numpy as np


async def run():

    arguments = parser.parse_args()
    working_path = "experiments/bodies/"
    robot_file_path = working_path + arguments.robot_name + ".yaml"
    # Parse command line / file input arguments
    settings = parser.parse_args()

    print("Starting Args:", settings)

    # Start Simulator
    if settings.simulator_cmd != 'debug':
        simulator_supervisor = DynamicSimSupervisor(
            world_file=settings.world,
            simulator_cmd=settings.simulator_cmd,
            simulator_args=["--verbose"],
            plugins_dir_path=os.path.join('.', 'build', 'lib'),
            models_dir_path=os.path.join('.', 'models'),
            simulator_name='gazebo'
        )
        await simulator_supervisor.launch_simulator(port=settings.port_start)
        await asyncio.sleep(0.1)

    # Connect to the simulator and pause
    connection = await World.create(settings, world_address=('127.0.0.1', settings.port_start))
    await asyncio.sleep(1)

    # Starts the simulation
    await connection.pause(False)

    # Load a robot from yaml
    robot = revolve_bot.RevolveBot()
    robot.load_file(robot_file_path)
    robot.update_substrate()

    # Insert the robot in the simulator
    robot_manager = await connection.insert_robot(robot, Vector3(0, 0, settings.z_start))

    # Start a run loop to do some stuff
    status = 'dead' if robot_manager.dead else 'alive'

    while status is 'alive':
        status = 'dead' if robot_manager.dead else 'alive'
        if robot_manager._orientations:
            orientation_roll = []
            orientation_pitch =[]
            for i in robot_manager._orientations:
                orientation_roll.append(i[0])
                orientation_pitch.append(i[1])
            if max(map(abs, orientation_roll)) > 2.4:
                print("They see me rolling! They hating")
            if max(map(abs, orientation_pitch)) > 2.4:
                print("It is pitching!")
        print(f"Robot is {status}")
        print(f"The balance is: {Measures.head_balance(robot_manager)}")
        await asyncio.sleep(1)

    fitnesses = fitness.displacement_velocity(managers[i], robot[i])
    fitnesses_hill = fitness.displacement_velocity_hill(managers[i], robot[i])
    out_data = [fitnesses, fitnesses_hill]

    print(f"**The Data is: {out_data} \n")