/**
 * 
 */
package utilities;

/**
 * Gestione Cache, interfaccia applicativa
 * 
 * @author giampietro Zedda
 * @version 1.0
 *
 *
 */
public interface ICache<K, T> {

    void put(K key, T value) ;
   
    T get(K key);

    void remove(K key);
 
    long size();
    
    void cleanup();    
}
