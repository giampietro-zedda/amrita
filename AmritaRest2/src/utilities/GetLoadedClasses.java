package utilities;
import java.io.PrintWriter;
import java.lang.instrument.Instrumentation;
import java.lang.management.ManagementFactory;
import java.lang.management.MemoryPoolMXBean;
import java.lang.management.MemoryUsage;
import java.lang.management.ThreadMXBean;
import java.util.List;

public class GetLoadedClasses {
  
  public static void main(String[] a){
    //premain();
    
  }
  
  public static void premain(final Instrumentation inst) {
    Runtime.getRuntime().addShutdownHook(new Thread() {
      public void run() {
        try {
          PrintWriter out = new PrintWriter(System.err);
          
          ThreadMXBean tb = ManagementFactory.getThreadMXBean();
          out.printf("Current thread count: %d%n", tb.getThreadCount());
          out.printf("Peak thread count: %d%n", tb.getPeakThreadCount());

          List<MemoryPoolMXBean> pools = ManagementFactory.getMemoryPoolMXBeans();
          for (MemoryPoolMXBean pool : pools) {
            MemoryUsage peak = pool.getPeakUsage();
            out.printf("Peak %s memory used: %,d%n", pool.getName(), peak.getUsed());
            out.printf("Peak %s memory reserved: %,d%n", pool.getName(), peak.getCommitted());
          }

          Class[] loaded = inst.getAllLoadedClasses();
          out.println("Loaded classes:");
          for (Class c : loaded)
            out.println(c.getName());
          out.close();
        } catch (Throwable t) {
          System.err.println("Exception in agent: " + t);
        }
      }
    });
  }
}