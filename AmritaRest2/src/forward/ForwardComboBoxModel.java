
package forward;
import java.util.ArrayList;
import javax.swing.DefaultComboBoxModel;

/**
 * copyright (c) 2009-2012 e-Amrita - Ing Giampietro Zedda   Turin (Italy)
 * 
 * <h1>
 * ForwardComboBoxModel
 * </h1>
 * <p>
 * Holds additional data for a {@link JComboBox} swing control useful for applications and completely user dependent.<br>
 * Often it's showed a list of items in the comboBox but structured information must be maintened for each item,<br>
 * like a key or a numeric valueapplication dependente.<br>
 * It's managed by this class an object for each item in the list and it's responsability of application manage the<br>
 * array of ojects when inserting, removing and updating items int the list.<br>
 * <p>
 * In any case item and objects bound must hold the same index.
 * <br>
 *
 * @author Giampietro Zedda
 * @version 1.0.0
 * @since 09/dic/2011 
 *
*/
@SuppressWarnings("rawtypes")
public class ForwardComboBoxModel extends DefaultComboBoxModel {

	private static final long serialVersionUID = 1L;
    private ArrayList<Object> al_dataBound = null;				// One element for each list item
    private boolean isComboDataBound = false;					// Binding active or not
    
	public ForwardComboBoxModel() {
		super();
		al_dataBound = new ArrayList<Object> ();
	}
	
	/**
	 * Gets the structure twin of items list, with application bound objects.
	 * <br>
	 * @return the al_dataBound
	 */
	public ArrayList<Object> _getAllDataBound() {
		return al_dataBound;
	}
	
	/**
	 * Gets the application object data bound to a comboBox item.
	 * <p>
	 * If the index is out of bound returns null.<br>
	 * <p>
	 * @param index the item comboBox index 0-based
	 * @return the object bound to item
	 */
	public Object _getDataBound(int index) {
		if (index >= al_dataBound.size()) {return null;}
		return al_dataBound.get(index);
	}

	/**
	 * Add an item to the comboBox setting the bound object to null.
	 * <br>
	 * @param object the object to append to comboBox
	 */
	@SuppressWarnings("unchecked")
	public void _addElement(Object object) {
		addElement(object);
		al_dataBound.add(null);
		return;
	}

	/**
	 * Add an element to the comboBox setting the bound object too.
	 * <br>
	 * @param object the object to append to comboBox
	 * @param objectBound the object to bind to the item
	 */
	@SuppressWarnings("unchecked")
	public void _addElement(Object object, Object objectBound) {
		addElement(object);
		al_dataBound.add(objectBound);
		this.isComboDataBound = true;
		return;
	}

	/**
	 * Sets the structure twin of items list, with application objects.
	 * <br>
	 * All bound objects will be replaced.<b>
	 * The number of objects bound must be the same of items.<br>+
	 * <p>
	 * @param al_dataBound the al_dataBound to set
	 */
	public void _setDataBoundStructure(ArrayList<Object> al_dataBound) {
		this.al_dataBound = al_dataBound;
		this.isComboDataBound = true;
	}
	
	/**
	 * Sets the application object bound to an item.
	 * <br>
	 * If the item index is out of bound no action will be taken.<b>
	 * <p>
	 * @param objectBound the application object to set
	 */
	public void _setDataBound(int index, Object objectBound) {
		if (index >= al_dataBound.size()) {return;}
		al_dataBound.set(index, objectBound);
	}
	
	/**  
	 * Gets if the list hold an object for any item.<br>
	 * <p>
	 * @return the isComboDataBound
	 */
	public boolean _isComboDataBound() {
		return isComboDataBound;
	}
	/**
	 * Gets if the list hold an object for any item.<br>
	 * <p>
	 * @param isComboDataBound the isComboDataBound to set
	 */
	public void _setComboDataBound(boolean isComboDataBound) {
		this.isComboDataBound = isComboDataBound;
	}
}
