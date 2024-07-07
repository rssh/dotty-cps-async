package snippets.gsoc2024



/*
import cps.*

def someMethod(data: Data): EffectSystem[Error::*Async::*Env[Database,Network,Auth]] =
  reify[[X] =>> Error::*Async::*Env[Database,Network,Auth] ]  {
    val associatedData = reflect(summon[DB].query(makeRequest(data)))
    if (associatedData.isEmpty) {
        error("No data found")
    }
    val result = reflect(summon[Network].send(associatedData))
    ensure(result.status.isOk)
    reflect(summon[Auth].authorize(result))
  }
  
 */