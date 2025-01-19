package cps.plugin

class CpsPluginSettings(
    var useLoom: Boolean = true,
    var debugLevel: Int = 0,
    var printTree: Boolean = false,
    var printCode: Boolean = false,
    var withShiftReplaceStages: Boolean = false,
    var transformDirectContextLambda: Boolean = false,

    /** If set to true, then if RuntimeAeait is present then do not search for shifted functions substitutions where await is
      * encoured in the arguments of hisht-order function, instead always use RuntimeAwait.
      */
    var runtimeAwaitBeforeCps: Boolean = true
)
