package dao;

import java.sql.Connection;

import analyzer.UserConfiguration;
import entities.EntitySqlGeneric;

public class MySQLDAOFactory extends DAOFactory {

    public final int SQL_CODE_DUPLICATE = 1062;                  // It depends on database type
    
    private DAOImplUser DAOImplUser = null;
    private DAOImplObject DAOImplObject = null;
    private DAOImplObjectShort DAOImplObjectShort = null;
    private DAOImplObjectOption DAOImplObjectOption = null;
    private DAOImplRelation DAOImplRelation = null;
    private DAOImplRelationOrigin DAOImplRelationOrigin = null;
    private DAOImplCopyEntityDefinition DAOImplCopyEntityDefinition = null;
    private DAOImplDynamicField DAOImplDynamicField = null;
    private DAOImplDynamicFieldSub DAOImplDynamicFieldSub = null;
    private DAOImplDynamicFieldSubSetting DAOImplDynamicFieldSubSetting = null;
    private DAOImplDynamicFieldSubValue DAOImplDynamicFieldSubValue = null;
    private DAOImplDynamicFieldSubWaitExt DAOImplDynamicFieldSubWaitExt = null;
    private DAOImplDynamicCicsMapping DAOImplDynamicCicsMapping = null; 
    private DAOImplImpactPlan DAOImplImpactPlan = null;    
    private DAOImplImpactObject DAOImplImpactObject = null;    
    private DAOImplImpactCompile DAOImplImpactCompile = null;    
    private DAOImplWhereUsedItem DAOImplWhereUsedItem = null;
    private DAOImplObjectAnalysisError DAOImplObjectAnalysisError = null;
    private DAOImplObjectAnalysisInfo DAOImplObjectAnalysisInfo = null;
    private DAOImplMetricScenario DAOImplMetricScenario = null;
    private DAOImplMetricValue DAOImplMetricValue = null;
    private DAOImplMetricViolationConfig DAOImplMetricViolationConfig = null;
    private DAOImplMetricViolation DAOImplMetricViolation = null;
    private DAOImplSqlGeneric DAOImplSqlGeneric = null;
    private DAOImplIndexItem DAOImplIndexItem = null;
    private DAOImplDynamicValueExt DAOImplDynamicValueExt = null;
    private DAOImplTagValue DAOImplTagValue = null;
    private DAOImplProcessLog DAOImplProcessLog = null;
    private DAOImplMapDescriptor DAOImplMapDescriptor = null;
    private DAOImplMapItem DAOImplMapItem = null;
     /**
     * Metodo per creare una connessione sul DB MySQL
     * 
     * @return l'oggetto Connection.
     */
 
 
    /* Get DAO Factory for User, create connection and close after operation*/
	public IDAOUser getDAOUser(Connection conn, boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplUser == null) {
			DAOImplUser = new DAOImplUser(conn, isConnToRelease, isCommitAfterUpdates, ucfg);
		} else {
			DAOImplUser.setConn(conn);
		}
		return DAOImplUser;
	}

    /* Get DAO Factory for User, create connection and close after operation*/
	public IDAOUser getDAOUser(boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplUser == null) {
			DAOImplUser = new DAOImplUser(isConnToRelease, isCommitAfterUpdates, ucfg);
		}
		return DAOImplUser;
	}

    /* Get DAO Factory for Object, create connection and close after operation*/
	public IDAOObject getDAOObject(Connection conn, boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplObject == null) {
			DAOImplObject =  new DAOImplObject(conn, isConnToRelease, isCommitAfterUpdates, ucfg);
		} else {
			DAOImplObject.setConn(conn);
		}
		return DAOImplObject;
	}

    /* Get DAO Factory for Object, create connection and close after operation*/
	public IDAOObject getDAOObject(boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplObject == null) {
			DAOImplObject =  new DAOImplObject(isConnToRelease, isCommitAfterUpdates, ucfg);
		}
		return DAOImplObject;
	}

    /* Get DAO Factory for Object, create connection and close after operation*/
	public DAOImplObjectShort getDAOObjectShort(Connection conn, boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplObjectShort == null) {
			DAOImplObjectShort =  new DAOImplObjectShort(conn, isConnToRelease, isCommitAfterUpdates, ucfg);
		} else {
			DAOImplObjectShort.setConn(conn);
		}
		return DAOImplObjectShort;
	}
	
    /* Get DAO Factory for Object, create connection and close after operation*/
	public DAOImplObjectShort getDAOObjectShort(boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplObjectShort == null) {
			DAOImplObjectShort =  new DAOImplObjectShort(isConnToRelease, isCommitAfterUpdates, ucfg);
		}
		return DAOImplObjectShort;
	}
	
    /* Get DAO Factory for ObjectOption, create connection and close after operation*/
	public IDAOObjectOption getDAOObjectOption(Connection conn, boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplObjectOption == null) {
			DAOImplObjectOption =  new DAOImplObjectOption(conn, isConnToRelease, isCommitAfterUpdates, ucfg);
		} else {
			DAOImplObjectOption.setConn(conn);
		}
		return DAOImplObjectOption;
	}

    /* Get DAO Factory for ObjectOption, create connection and close after operation*/
	public IDAOObjectOption getDAOObjectOption(boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplObjectOption == null) {
			DAOImplObjectOption =  new DAOImplObjectOption(isConnToRelease, isCommitAfterUpdates, ucfg);
		}
		return DAOImplObjectOption;
	}

    /* Get DAO Factory for ObjectOption, create connection and close after operation*/
	public IDAORelation getDAORelation(Connection conn, boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplRelation == null) {
			DAOImplRelation =  new DAOImplRelation(conn, isConnToRelease, isCommitAfterUpdates, ucfg);
		} else {
			DAOImplRelation.setConn(conn);
		}
		return DAOImplRelation;
	}
	
    /* Get DAO Factory for ObjectOption, create connection and close after operation*/
	public IDAORelation getDAORelation(boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplRelation == null) {
			DAOImplRelation =  new DAOImplRelation(isConnToRelease, isCommitAfterUpdates, ucfg);
		}
		return DAOImplRelation;
	}
	
    /* Get DAO Factory for ObjectOption, create connection and close after operation*/
	public IDAORelationOrigin getDAORelationOrigin(Connection conn, boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplRelationOrigin == null) {
			DAOImplRelationOrigin =  new DAOImplRelationOrigin(conn, isConnToRelease, isCommitAfterUpdates, ucfg);
		} else {
			DAOImplRelationOrigin.setConn(conn);
		}
		return DAOImplRelationOrigin;
	}
	
	public IDAORelationOrigin getDAORelationOrigin(boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplRelationOrigin == null) {
			DAOImplRelationOrigin =  new DAOImplRelationOrigin(isConnToRelease, isCommitAfterUpdates, ucfg);
		}
		return DAOImplRelationOrigin;
	}
	
    /* Get DAO Factory for CopyEntityDefinition, create connection and close after operation*/
	public IDAOCopyEntityDefinition getDAOCopyEntityDefinition(Connection conn, boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplCopyEntityDefinition == null) {
			DAOImplCopyEntityDefinition =  new DAOImplCopyEntityDefinition(conn, isConnToRelease, isCommitAfterUpdates, ucfg);
		} else {
			DAOImplCopyEntityDefinition.setConn(conn);
		}
		return DAOImplCopyEntityDefinition;
	}

    /* Get DAO Factory for CopyEntityDefinition, create connection and close after operation*/
	public IDAOCopyEntityDefinition getDAOCopyEntityDefinition(boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplCopyEntityDefinition == null) {
			DAOImplCopyEntityDefinition =  new DAOImplCopyEntityDefinition(isConnToRelease, isCommitAfterUpdates, ucfg);
		}
		return DAOImplCopyEntityDefinition;
	}

	
    /* Get DAO Factory for DynamicField, create connection and close after operation*/
	public IDAODynamicField getDAODynamicField(Connection conn, boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplDynamicField == null) {
			DAOImplDynamicField =  new DAOImplDynamicField(conn, isConnToRelease, isCommitAfterUpdates, ucfg);
		} else {
			DAOImplDynamicField.setConn(conn);
		}
		return DAOImplDynamicField;
	}

    /* Get DAO Factory for DynamicField, create connection and close after operation*/
	public IDAODynamicField getDAODynamicField(boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplDynamicField == null) {
			DAOImplDynamicField =  new DAOImplDynamicField(isConnToRelease, isCommitAfterUpdates, ucfg);
		}
		return DAOImplDynamicField;
	}

    /* Get DAO Factory for DynamicFieldSub, create connection and close after operation*/
	public IDAODynamicFieldSub getDAODynamicFieldSub(Connection conn, boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplDynamicFieldSub == null) {
			DAOImplDynamicFieldSub =  new DAOImplDynamicFieldSub(conn, isConnToRelease, isCommitAfterUpdates, ucfg);
		} else {
			DAOImplDynamicFieldSub.setConn(conn);
		}
		return DAOImplDynamicFieldSub;
	}

    /* Get DAO Factory for DynamicFieldSub, create connection and close after operation*/
	public IDAODynamicFieldSub getDAODynamicFieldSub(boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplDynamicFieldSub == null) {
			DAOImplDynamicFieldSub =  new DAOImplDynamicFieldSub(isConnToRelease, isCommitAfterUpdates, ucfg);
		}
		return DAOImplDynamicFieldSub;
	}

    /* Get DAO Factory for DynamicFieldSub, create connection and close after operation*/
	public IDAODynamicFieldSubSetting getDAODynamicFieldSubSetting(Connection conn, boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplDynamicFieldSubSetting == null) {
			DAOImplDynamicFieldSubSetting =  new DAOImplDynamicFieldSubSetting(conn, isConnToRelease, isCommitAfterUpdates, ucfg);
		} else {
			DAOImplDynamicFieldSubSetting.setConn(conn);
		}
		return DAOImplDynamicFieldSubSetting;
	}

    /* Get DAO Factory for DynamicFieldSub, create connection and close after operation*/
	public IDAODynamicFieldSubSetting getDAODynamicFieldSubSetting(boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplDynamicFieldSubSetting == null) {
			DAOImplDynamicFieldSubSetting =  new DAOImplDynamicFieldSubSetting(isConnToRelease, isCommitAfterUpdates, ucfg);
		}
		return DAOImplDynamicFieldSubSetting;
	}

  /* Get DAO Factory for DynamicFieldSubValue, create connection and close after operation*/
	public IDAODynamicFieldSubValue getDAODynamicFieldSubValue(Connection conn, boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplDynamicFieldSubValue == null) {
			DAOImplDynamicFieldSubValue =  new DAOImplDynamicFieldSubValue(conn, isConnToRelease, isCommitAfterUpdates, ucfg);
		} else {
			DAOImplDynamicFieldSubValue.setConn(conn);
		}
		return DAOImplDynamicFieldSubValue;
	}
	
    /* Get DAO Factory for DynamicFieldSubValue, create connection and close after operation*/
	public IDAODynamicFieldSubValue getDAODynamicFieldSubValue(boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplDynamicFieldSubValue == null) {
			DAOImplDynamicFieldSubValue =  new DAOImplDynamicFieldSubValue(isConnToRelease, isCommitAfterUpdates, ucfg);
		}
		return DAOImplDynamicFieldSubValue;
	}
	
    /* Get DAO Factory for DynamicFieldSubwAITeXT, create connection and close after operation*/
	public IDAODynamicFieldSubWaitExt getDAODynamicFieldSubWaitExt(Connection conn, boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplDynamicFieldSubWaitExt == null) {
			DAOImplDynamicFieldSubWaitExt =  new DAOImplDynamicFieldSubWaitExt(conn, isConnToRelease, isCommitAfterUpdates, ucfg);
		} else {
			DAOImplDynamicFieldSubWaitExt.setConn(conn);
		}
		return DAOImplDynamicFieldSubWaitExt;
	}
	
    /* Get DAO Factory for DynamicFieldSubwAITeXT, create connection and close after operation*/
	public IDAODynamicFieldSubWaitExt getDAODynamicFieldSubWaitExt(boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplDynamicFieldSubWaitExt == null) {
			DAOImplDynamicFieldSubWaitExt =  new DAOImplDynamicFieldSubWaitExt(isConnToRelease, isCommitAfterUpdates, ucfg);
		}
		return DAOImplDynamicFieldSubWaitExt;
	}

    /* Get DAO Factory for DynamicCicsMapping, create connection and close after operation*/
	public IDAODynamicCicsMapping getDAODynamicCicsMapping(Connection conn, boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplDynamicCicsMapping == null) {
			DAOImplDynamicCicsMapping =  new DAOImplDynamicCicsMapping(conn, isConnToRelease, isCommitAfterUpdates, ucfg);
		} else {
			DAOImplDynamicCicsMapping.setConn(conn);
		}
		return DAOImplDynamicCicsMapping;
	}

    /* Get DAO Factory for DynamicCicsMapping, create connection and close after operation*/
	public IDAODynamicCicsMapping getDAODynamicCicsMapping(boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplDynamicCicsMapping == null) {
			DAOImplDynamicCicsMapping =  new DAOImplDynamicCicsMapping(isConnToRelease, isCommitAfterUpdates, ucfg);
		}
		return DAOImplDynamicCicsMapping;
	}

    /* Get DAO Factory for DynamicCicsMapping, create connection and close after operation*/
	public IDAOImpactPlan getDAOImpactPlan(Connection conn, boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplImpactPlan == null) {
			DAOImplImpactPlan =  new DAOImplImpactPlan(conn, isConnToRelease, isCommitAfterUpdates, ucfg);
		} else {
			DAOImplImpactPlan.setConn(conn);
		}
		return DAOImplImpactPlan;
	}

    /* Get DAO Factory for ImpactPlan, create connection and close after operation*/
	public IDAOImpactPlan getDAOImpactPlan(boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplImpactPlan == null) {
			DAOImplImpactPlan =  new DAOImplImpactPlan(isConnToRelease, isCommitAfterUpdates, ucfg);
		}
		return DAOImplImpactPlan;
	}
	
    /* Get DAO Factory for DynamicCicsMapping, create connection and close after operation*/
	public IDAOImpactObject getDAOImpactObject(Connection conn, boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplImpactObject == null) {
			DAOImplImpactObject =  new DAOImplImpactObject(conn, isConnToRelease, isCommitAfterUpdates, ucfg);
		} else {
			DAOImplImpactObject.setConn(conn);
		}
		return DAOImplImpactObject;
	}

    /* Get DAO Factory for ImpactObject, create connection and close after operation*/
	public IDAOImpactObject getDAOImpactObject(boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplImpactObject == null) {
			DAOImplImpactObject =  new DAOImplImpactObject(isConnToRelease, isCommitAfterUpdates, ucfg);
		}
		return DAOImplImpactObject;
	}
	
    /* Get DAO Factory for DynamicCicsMapping, create connection and close after operation*/
	public IDAOImpactCompile getDAOImpactCompile(Connection conn, boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplImpactCompile == null) {
			DAOImplImpactCompile =  new DAOImplImpactCompile(conn, isConnToRelease, isCommitAfterUpdates, ucfg);
		} else {
			DAOImplImpactCompile.setConn(conn);
		}
		return DAOImplImpactCompile;
	}

    /* Get DAO Factory for ImpactCompile, create connection and close after operation*/
	public IDAOImpactCompile getDAOImpactCompile(boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplImpactCompile == null) {
			DAOImplImpactCompile =  new DAOImplImpactCompile(isConnToRelease, isCommitAfterUpdates, ucfg);
		}
		return DAOImplImpactCompile;
	}
	
  /* Get DAO Factory for WhereUsedItem, create connection and close after operation*/
	public IDAOWhereUsedItem getDAOWhereUsedItem(Connection conn, boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplWhereUsedItem == null) {
			DAOImplWhereUsedItem =  new DAOImplWhereUsedItem(conn, isConnToRelease, isCommitAfterUpdates, ucfg);
		} else {
			DAOImplWhereUsedItem.setConn(conn);
		}
		return DAOImplWhereUsedItem;
	}

    /* Get DAO Factory for WhereUsedItem, create connection and close after operation*/
	public IDAOWhereUsedItem getDAOWhereUsedItem(boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplWhereUsedItem == null) {
			DAOImplWhereUsedItem =  new DAOImplWhereUsedItem(isConnToRelease, isCommitAfterUpdates, ucfg);
		}
		return DAOImplWhereUsedItem;
	}

    /* Get DAO Factory for ObjectAnalysisError, create connection and close after operation*/
	public IDAOObjectAnalysisError getDAOObjectAnalysisError(Connection conn, boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplObjectAnalysisError == null) {
			DAOImplObjectAnalysisError =  new DAOImplObjectAnalysisError(conn, isConnToRelease, isCommitAfterUpdates, ucfg);
		} else {
			DAOImplObjectAnalysisError.setConn(conn);
		}
		return DAOImplObjectAnalysisError;
	}

    /* Get DAO Factory for ObjectAnalysisError, create connection and close after operation*/
	public IDAOObjectAnalysisError getDAOObjectAnalysisError(boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplObjectAnalysisError == null) {
			DAOImplObjectAnalysisError =  new DAOImplObjectAnalysisError(isConnToRelease, isCommitAfterUpdates, ucfg);
		}
		return DAOImplObjectAnalysisError;
	}

	   /* Get DAO Factory for ObjectAnalysisInfo, create connection and close after operation*/
		public IDAOObjectAnalysisInfo getDAOObjectAnalysisInfo(Connection conn, boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
			if (DAOImplObjectAnalysisInfo == null) {
				DAOImplObjectAnalysisInfo =  new DAOImplObjectAnalysisInfo(conn, isConnToRelease, isCommitAfterUpdates, ucfg);
			} else {
				DAOImplObjectAnalysisInfo.setConn(conn);
			}
			return DAOImplObjectAnalysisInfo;
		}

		   /* Get DAO Factory for ObjectAnalysisInfo, create connection and close after operation*/
		public IDAOObjectAnalysisInfo getDAOObjectAnalysisInfo(boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
			if (DAOImplObjectAnalysisInfo == null) {
				DAOImplObjectAnalysisInfo =  new DAOImplObjectAnalysisInfo(isConnToRelease, isCommitAfterUpdates, ucfg);
			}
			return DAOImplObjectAnalysisInfo;
		}

	    /* Get DAO Factory for MetricScenario, create connection and close after operation*/
		public IDAOMetricScenario getDAOMetricScenario(Connection conn, boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
			if (DAOImplMetricScenario == null) {
				DAOImplMetricScenario =  new DAOImplMetricScenario(conn, isConnToRelease, isCommitAfterUpdates, ucfg);
			} else {
				DAOImplMetricScenario.setConn(conn);
			}
			return DAOImplMetricScenario;
		}

	    /* Get DAO Factory for MetricScenario, create connection and close after operation*/
		public IDAOMetricScenario getDAOMetricScenario(boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
			if (DAOImplMetricScenario == null) {
				DAOImplMetricScenario =  new DAOImplMetricScenario(isConnToRelease, isCommitAfterUpdates, ucfg);
			}
			return DAOImplMetricScenario;
		}

		
    /* Get DAO Factory for MetricValue, create connection and close after operation*/
	public IDAOMetricValue getDAOMetricValue(Connection conn, boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplMetricValue == null) {
			DAOImplMetricValue =  new DAOImplMetricValue(conn, isConnToRelease, isCommitAfterUpdates, ucfg);
		} else {
			DAOImplMetricValue.setConn(conn);
		}
		return DAOImplMetricValue;
	}

    /* Get DAO Factory for MetricValue, create connection and close after operation*/
	public IDAOMetricValue getDAOMetricValue(boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplMetricValue == null) {
			DAOImplMetricValue =  new DAOImplMetricValue(isConnToRelease, isCommitAfterUpdates, ucfg);
		}
		return DAOImplMetricValue;
	}

    /* Get DAO Factory for MetricViolationConfig, create connection and close after operation*/
	public IDAOMetricViolationConfig getDAOMetricViolationConfig(Connection conn, boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplMetricViolationConfig == null) {
			DAOImplMetricViolationConfig =  new DAOImplMetricViolationConfig(conn, isConnToRelease, isCommitAfterUpdates, ucfg);
		} else {
			DAOImplMetricViolationConfig.setConn(conn);
		}
		return DAOImplMetricViolationConfig;
	}

    /* Get DAO Factory for MetricViolationConfig, create connection and close after operation*/
	public IDAOMetricViolationConfig getDAOMetricViolationConfig(boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplMetricViolationConfig == null) {
			DAOImplMetricViolationConfig =  new DAOImplMetricViolationConfig(isConnToRelease, isCommitAfterUpdates, ucfg);
		}
		return DAOImplMetricViolationConfig;
	}


	
    /* Get DAO Factory for MetricViolation, create connection and close after operation*/
	public IDAOMetricViolation getDAOMetricViolation(Connection conn, boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplMetricViolation == null) {
			DAOImplMetricViolation =  new DAOImplMetricViolation(conn, isConnToRelease, isCommitAfterUpdates, ucfg);
		} else {
			DAOImplMetricViolation.setConn(conn);
		}
		return DAOImplMetricViolation;
	}

    /* Get DAO Factory for MetricViolation, create connection and close after operation*/
	public IDAOMetricViolation getDAOMetricViolation(boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplMetricViolation == null) {
			DAOImplMetricViolation =  new DAOImplMetricViolation(isConnToRelease, isCommitAfterUpdates, ucfg);
		}
		return DAOImplMetricViolation;
	}

    /* Get DAO Factory for MetricViolation, create connection and close after operation*/
	/* Version returning a set of objects */
	public IDAOSqlGeneric<EntitySqlGeneric> getDAOSqlGeneric(Connection conn, boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplSqlGeneric == null) {
			DAOImplSqlGeneric =  new DAOImplSqlGeneric(conn, isConnToRelease, isCommitAfterUpdates, ucfg);
		} else {
			DAOImplSqlGeneric.setConn(conn);
		}
		return DAOImplSqlGeneric;
	}

    /* Get DAO Factory for MetricViolation, create connection and close after operation*/
	/* Version returning a set of objects */
	public IDAOSqlGeneric<EntitySqlGeneric> getDAOSqlGeneric(boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplSqlGeneric == null) {
			DAOImplSqlGeneric =  new DAOImplSqlGeneric(isConnToRelease, isCommitAfterUpdates, ucfg);
		}
		return DAOImplSqlGeneric;
	}
	
    /* Get DAO Factory for IndexItem, create connection and close after operation*/
	public IDAOIndexItem getDAOIndexItem(Connection conn, boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplIndexItem == null) {
			DAOImplIndexItem =  new DAOImplIndexItem(conn, isConnToRelease, isCommitAfterUpdates, ucfg);
		} else {
			DAOImplIndexItem.setConn(conn);
		}
		return DAOImplIndexItem;
	}
  
    /* Get DAO Factory for IndexItem, create connection and close after operation*/
	public IDAOIndexItem getDAOIndexItem(boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplIndexItem == null) {
			DAOImplIndexItem =  new DAOImplIndexItem(isConnToRelease, isCommitAfterUpdates, ucfg);
		}
		return DAOImplIndexItem;
	}
  
    /* Get DAO Factory for DynamicValueExt, create connection and close after operation*/
	public IDAODynamicValueExt getDAODynamicValueExt(Connection conn, boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplDynamicValueExt == null) {
			DAOImplDynamicValueExt =  new DAOImplDynamicValueExt(conn, isConnToRelease, isCommitAfterUpdates, ucfg);
		} else {
			DAOImplDynamicValueExt.setConn(conn);
		}
		return DAOImplDynamicValueExt;
	}
  
    /* Get DAO Factory for DynamicValueExt, create connection and close after operation*/
	public IDAODynamicValueExt getDAODynamicValueExt(boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplDynamicValueExt == null) {
			DAOImplDynamicValueExt =  new DAOImplDynamicValueExt(isConnToRelease, isCommitAfterUpdates, ucfg);
		}
		return DAOImplDynamicValueExt;
	}
  
    /* Get DAO Factory for TagValue, create connection and close after operation*/
	public IDAOTagValue getDAOTagValue(Connection conn, boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplTagValue == null) {
			DAOImplTagValue =  new DAOImplTagValue(conn, isConnToRelease, isCommitAfterUpdates, ucfg);
		} else {
			DAOImplTagValue.setConn(conn);
		}
		return DAOImplTagValue;
	}

    /* Get DAO Factory for TagValue, create connection and close after operation*/
	public IDAOTagValue getDAOTagValue(boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplTagValue == null) {
			DAOImplTagValue =  new DAOImplTagValue(isConnToRelease, isCommitAfterUpdates, ucfg);
		}
		return DAOImplTagValue;
	}

    /* Get DAO Factory for TagValue, create connection and close after operation*/
	public IDAOProcessLog getDAOProcessLog(Connection conn, boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplProcessLog == null) {
			DAOImplProcessLog =  new DAOImplProcessLog(conn, isConnToRelease, isCommitAfterUpdates, ucfg);
		} else {
			DAOImplProcessLog.setConn(conn);
		}
		return DAOImplProcessLog;
	}

    /* Get DAO Factory for TagValue, create connection and close after operation*/
	public IDAOProcessLog getDAOProcessLog(boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplProcessLog == null) {
			DAOImplProcessLog =  new DAOImplProcessLog(isConnToRelease, isCommitAfterUpdates, ucfg);
		}
		return DAOImplProcessLog;
	}

   /* Get DAO Factory for MapDescriptor, create connection and close after operation*/
	public IDAOMapDescriptor getDAOMapDescriptor(Connection conn, boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplMapDescriptor == null) {
			DAOImplMapDescriptor =  new DAOImplMapDescriptor(conn, isConnToRelease, isCommitAfterUpdates, ucfg);
		} else {
			DAOImplMapDescriptor.setConn(conn);
		}
		return DAOImplMapDescriptor;
	}

    /* Get DAO Factory for MapDescriptor, create connection and close after operation*/
	public IDAOMapDescriptor getDAOMapDescriptor(boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplMapDescriptor == null) {
			DAOImplMapDescriptor =  new DAOImplMapDescriptor(isConnToRelease, isCommitAfterUpdates, ucfg);
		}
		return DAOImplMapDescriptor;
	}

    /* Get DAO Factory for MapItem, create connection and close after operation*/
	public IDAOMapItem getDAOMapItem(Connection conn, boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplMapItem == null) {
			DAOImplMapItem =  new DAOImplMapItem(conn, isConnToRelease, isCommitAfterUpdates, ucfg);
		} else {
			DAOImplMapItem.setConn(conn);
		}
		return DAOImplMapItem;
	}

    /* Get DAO Factory for MapItem, create connection and close after operation*/
	public IDAOMapItem getDAOMapItem(boolean isConnToRelease, boolean isCommitAfterUpdates, UserConfiguration ucfg) {
		if (DAOImplMapItem == null) {
			DAOImplMapItem =  new DAOImplMapItem(isConnToRelease, isCommitAfterUpdates, ucfg);
		}
		return DAOImplMapItem;
	}


}
