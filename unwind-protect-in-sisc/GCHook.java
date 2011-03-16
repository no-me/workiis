import sisc.data.Procedure;
import sisc.data.Value;
import sisc.interpreter.AppContext;
import sisc.interpreter.Context;
import sisc.interpreter.Interpreter;
import sisc.interpreter.SchemeCaller;
import sisc.interpreter.SchemeException;

/* Calls the given proc when instances of this class is about to be finalised */
public class GCHook implements SchemeCaller
{
  private Procedure proc;
  private AppContext appContext;
  private boolean isPerform;

  /* proc is the Procedure to be executed when this instance is going to be
     finalised */
  public GCHook(Procedure proc)
  {
    Interpreter current = Context.currentInterpreter();
    if (current == null) {
      // it is an external call (java->scheme call without preceeding scheme->java call 
      throw new IllegalStateException("GCHook can be instantiated only by internal call");
    }
    if (proc == null) {
      throw new IllegalArgumentException("proc must not be null");
    }
    this.proc = proc;
    appContext = current.getCtx();
    isPerform = true;
  }

  /* Cancels invocation of proc */
  public void cancel()
  {
    isPerform = false;
  }

  
  public Object execute(Interpreter interpreter) throws SchemeException
  {
    return interpreter.eval(proc, new Value[0]);
  }
  
  
  protected void finalize() throws Throwable
  {
    super.finalize();
    if (isPerform) {
      // This will indirectly call this.execute()
      Context.execute(appContext, this);
    }
  }
}
