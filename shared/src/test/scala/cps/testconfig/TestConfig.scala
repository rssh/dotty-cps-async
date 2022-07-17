package cps.testconfig

/**
 * Dummy testconfig, to make `import cps.testconfig.given' work.
 * In testconfig package we will set implicit values for macro configuration, 
 * which can be different for different subprojects, because we want to test
 * all possible configuration - so we have a project for configuration, which 
 * contains testconfig
 *  
 *  This is a temporary workarround, until scala.quoted.Quotes.CompilationInfoModule.XmacroSetting 
 * will be available in the release build.
 *
 **/
object DummyTestConfig

given DummyTestConfig.type = DummyTestConfig