# Generated by the protocol buffer compiler.  DO NOT EDIT!
# source: robot.proto

import sys
_b=sys.version_info[0]<3 and (lambda x:x) or (lambda x:x.encode('latin1'))
from google.protobuf import descriptor as _descriptor
from google.protobuf import message as _message
from google.protobuf import reflection as _reflection
from google.protobuf import symbol_database as _symbol_database
from google.protobuf import descriptor_pb2
# @@protoc_insertion_point(imports)

_sym_db = _symbol_database.Default()


from . import body_pb2 as body__pb2
from . import neural_net_pb2 as neural__net__pb2


DESCRIPTOR = _descriptor.FileDescriptor(
  name='robot.proto',
  package='revolve.msgs',
  syntax='proto2',
  serialized_pb=_b('\n\x0brobot.proto\x12\x0crevolve.msgs\x1a\nbody.proto\x1a\x10neural_net.proto\"a\n\x05Robot\x12\n\n\x02id\x18\x01 \x02(\x05\x12 \n\x04\x62ody\x18\x02 \x02(\x0b\x32\x12.revolve.msgs.Body\x12*\n\x05\x62rain\x18\x03 \x02(\x0b\x32\x1b.revolve.msgs.NeuralNetwork')
  ,
  dependencies=[body__pb2.DESCRIPTOR,neural__net__pb2.DESCRIPTOR,])
_sym_db.RegisterFileDescriptor(DESCRIPTOR)




_ROBOT = _descriptor.Descriptor(
  name='Robot',
  full_name='revolve.msgs.Robot',
  filename=None,
  file=DESCRIPTOR,
  containing_type=None,
  fields=[
    _descriptor.FieldDescriptor(
      name='id', full_name='revolve.msgs.Robot.id', index=0,
      number=1, type=5, cpp_type=1, label=2,
      has_default_value=False, default_value=0,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    _descriptor.FieldDescriptor(
      name='body', full_name='revolve.msgs.Robot.body', index=1,
      number=2, type=11, cpp_type=10, label=2,
      has_default_value=False, default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
    _descriptor.FieldDescriptor(
      name='brain', full_name='revolve.msgs.Robot.brain', index=2,
      number=3, type=11, cpp_type=10, label=2,
      has_default_value=False, default_value=None,
      message_type=None, enum_type=None, containing_type=None,
      is_extension=False, extension_scope=None,
      options=None),
  ],
  extensions=[
  ],
  nested_types=[],
  enum_types=[
  ],
  options=None,
  is_extendable=False,
  syntax='proto2',
  extension_ranges=[],
  oneofs=[
  ],
  serialized_start=59,
  serialized_end=156,
)

_ROBOT.fields_by_name['body'].message_type = body__pb2._BODY
_ROBOT.fields_by_name['brain'].message_type = neural__net__pb2._NEURALNETWORK
DESCRIPTOR.message_types_by_name['Robot'] = _ROBOT


Robot = _reflection.GeneratedProtocolMessageType('Robot', (_message.Message,), dict(
  DESCRIPTOR = _ROBOT,
  __module__ = 'robot_pb2'
  # @@protoc_insertion_point(class_scope:revolve.msgs.Robot)
  ))
_sym_db.RegisterMessage(Robot)


# @@protoc_insertion_point(module_scope)
