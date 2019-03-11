#!/usr/bin/env python3
import os
import sys
import asyncio

# Add `..` folder in search path
current_dir = os.path.dirname(os.path.abspath(__file__))
newpath = os.path.join(current_dir, '..', '..')
sys.path.append(newpath)

from pygazebo.pygazebo import DisconnectError

from pyrevolve import revolve_bot
from pyrevolve import parser
from pyrevolve.angle import Tree
from pyrevolve.convert.yaml import yaml_to_proto
from pyrevolve.sdfbuilder import Pose
from pyrevolve.sdfbuilder.math import Vector3
from pyrevolve.tol.manage import World


async def run():
    """
    The main coroutine, which is started below.
    """
    # Parse command line / file input arguments
    robot = revolve_bot.RevolveBot()
    robot.load_file("experiments/examples/yaml/spider.yaml")
    robot.save_file("/tmp/test.yaml")
    settings = parser.parse_args()

    world = await World.create(settings)
    await world.pause(True)
    delete_future = await world.delete_model(robot._id)
    await delete_future

    sdf_model = robot.to_sdf(nice_format='  ')
    print(sdf_model)
    with open('/tmp/test.sdf.xml', 'w') as sdf_file:
        sdf_file.write(str(sdf_model))

    await asyncio.sleep(1.5)
    insert_future = await world.insert_model(str(sdf_model))
    await insert_future

    await world.pause(True)

    # while True:
    #     await asyncio.sleep(1.0)


async def run_old():
    """
    The main coroutine, which is started below.
    """
    # Parse command line / file input arguments
    settings = parser.parse_args()

    with open("experiments/examples/yaml/snake.yaml", 'r') as yaml_file:
        bot_yaml = yaml_file.read()
    settings.genome = "\n".join(bot_yaml.splitlines()).replace("\'", "\"")

    world = await World.create(settings)

    # These are useful when working with YAML
    body_spec = world.builder.body_builder.spec
    brain_spec = world.builder.brain_builder.spec

    # Create a robot from YAML
    proto_bot = yaml_to_proto(
            body_spec=body_spec,
            nn_spec=brain_spec,
            yaml=bot_yaml)

    robot_tree = Tree.from_body_brain(
            body=proto_bot.body,
            brain=proto_bot.brain,
            body_spec=body_spec)
    pose = Pose(position=Vector3(0, 0, 0.05))
    future = await (world.insert_robot(
            py_bot=robot_tree,
            pose=pose,
            # name="robot_26"
    ))
    robot_manager = await future

    await world.pause(False)

    # Start a run loop to do some stuff
    while True:
        # Print robot fitness every second
        print("Robot fitness is {fitness}".format(
                fitness=robot_manager.fitness()))
        await asyncio.sleep(10.0)


def main():
    def handler(loop, context):
        exc = context['exception']
        if isinstance(exc, DisconnectError) \
                or isinstance(exc, ConnectionResetError):
            print("Got disconnect / connection reset - shutting down.")
            sys.exit(0)
        raise context['exception']

    try:
        loop = asyncio.get_event_loop()
        loop.set_exception_handler(handler)
        loop.run_until_complete(run())
    except KeyboardInterrupt:
        print("Got CtrlC, shutting down.")


if __name__ == '__main__':
    print("STARTING")
    main()
    print("FINISHED")
