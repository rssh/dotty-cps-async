package cps.plugin.forest.application


enum ShiftedArgumentsTypeParamsShape {
  case SAME_TYPEPARAMS, EXTRA_TYPEPARAM, EXTRA_TYPEPARAM_LIST
}

enum ShiftedArgumentsPlainParamsShape {
  case EXTRA_PARAM_LIST, EXTRA_FIRST_PARAM, SAME_PARAMS
}

case class ShiftedArgumentsShape(
                                  tp: ShiftedArgumentsTypeParamsShape,
                                  p: ShiftedArgumentsPlainParamsShape
                                       )

object ShiftedArgumentsShape {

  def same = ShiftedArgumentsShape(
      ShiftedArgumentsTypeParamsShape.SAME_TYPEPARAMS,
      ShiftedArgumentsPlainParamsShape.SAME_PARAMS
      )

  def extraLists = ShiftedArgumentsShape(
      ShiftedArgumentsTypeParamsShape.EXTRA_TYPEPARAM_LIST,
      ShiftedArgumentsPlainParamsShape.EXTRA_PARAM_LIST
  )

}