/**
 * 
 */
package utilities;
import java.util.ArrayList;
import org.apache.commons.collections.MapIterator;
import org.apache.commons.collections.map.LRUMap;

/**
 * Implementation for caching
 * 
 * @author giampietro Zedda
 * @version 1.0
 *
 */
public class AmritaCache <K, T> implements ICache<K, T> {

    	private LRUMap amritaCacheMap;
        private int maxItems;
	   	private long timeToLive;
	 
	    protected class AmritaCacheObject {
	        public long lastAccessed = System.currentTimeMillis();
	        public T value;
	 
	        protected AmritaCacheObject(T value) {
	            this.value = value;
	        }
	    }
	
	    public AmritaCache(long amritaTimeToLive, final long amritaTimerInterval, int maxItems) {
	        this.timeToLive = amritaTimeToLive * 1000;
	        this.maxItems = maxItems;
	        
	        amritaCacheMap = new LRUMap(maxItems);
	 
	        if (timeToLive > 0 && amritaTimerInterval > 0) {
	 
	            Thread t = new Thread(new Runnable() {
	                public void run() {
	                    while (true) {
	                        try {
	                            Thread.sleep(amritaTimerInterval * 1000);
	                        } catch (InterruptedException ex) {
	                        }
	                        cleanup();
	                    }
	                }
	            });
	 
	            t.setDaemon(true);
	            t.start();
	        }
	    }
	
	
	@Override
	public void put(K key, T value) {
        synchronized (amritaCacheMap) {
            amritaCacheMap.put(key, new AmritaCacheObject(value));
        }
    }

	@SuppressWarnings("unchecked")
	@Override
	public T get(K key) {
	      synchronized (amritaCacheMap) {
	            AmritaCacheObject c = (AmritaCacheObject) amritaCacheMap.get(key);
	 
	            if (c == null)
	                return null;
	            else {
	                c.lastAccessed = System.currentTimeMillis();
	                return c.value;
	            }
	        }	}

	@Override
	public void remove(K key) {
        synchronized (amritaCacheMap) {
            amritaCacheMap.remove(key);
        }	
    }

	@Override
	public long size() {
        synchronized (amritaCacheMap) {
            return amritaCacheMap.size();
        }	
    }

	@SuppressWarnings("unchecked")
	@Override
	public void cleanup() {

		if (size() < maxItems) {
			return;
		};
        long now = System.currentTimeMillis();
        ArrayList<K> deleteKey = null;
 

        synchronized (amritaCacheMap) {
            MapIterator itr = amritaCacheMap.mapIterator();
            deleteKey = new ArrayList<K>((amritaCacheMap.size() / 2) + 1);
            K key = null; 
            AmritaCacheObject c = null;
 
            while (itr.hasNext()) {
                key = (K) itr.next();
                c = (AmritaCacheObject) itr.getValue();
 
                if (c != null && (now > (timeToLive + c.lastAccessed))) {deleteKey.add(key);
                }
            }
        }
 
        for (K key : deleteKey) {
            synchronized (amritaCacheMap) {
                amritaCacheMap.remove(key);
            }
 
            Thread.yield();
        }
    }

}
