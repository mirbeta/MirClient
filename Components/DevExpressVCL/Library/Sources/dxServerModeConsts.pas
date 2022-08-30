{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressDataController                                    }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL            }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxServerModeConsts;

{$I cxVer.inc}

interface

const
  sdxMissingConnection = 'The Connection property is not specified';
  sdxMissingTableName = 'The TableName property is not specified';
  sdxMissingSQL = 'The SQL property is not specified';
  sdxMissingSQLAdapter = 'The SQLAdapter property is not specified';
  sdxPrimaryKeysNotFound = 'Primary keys cannot be found. Specify the KeyFieldNames property';
  sdxDataSourceOpen = 'Cannot perform this operation on an open data source';
  //internal errors
  sdxInternalErrorKeyAbsentFetchRowsAll = 'internal error: key is absent after successful FetchRowsAll';
  sdxInternalErrorKeyAbsentFetchRowsTop = 'internal error: key is absent after successful FetchRowsTop';
  sdxInternalErrorKeyAbsentDoFetchKeys = 'internal error: key is absent after successful DoFetchKeys';
  sdxInternalErrorResultValueNotFound = 'internal error: (row cannot be found by index)';
  sdxInternalErrorTakeLessFetchCount = 'internal error: (APureTake(%d) < AFetchCount(%d)) (AGroupInfo.ChildDataRowCount = %d)';
  sdxInternalErrorTakeGreaterChildDataRowCount = 'internal error: (APureTake(%d) > AGroupInfo.ChildDataRowCount(%d))';
  sdxInternalErrorSkipAndTakeWereNotChosen = 'internal error: neither skip nor take were chosen';
  sdxInternalErrorDoubleResult = 'internal error: (rowindex is duplicated)';
  //internal inconsistency
  sdxInconsistencyWrongNestedGroupsRowCount = 'The total row count of nested groups (%d) does not equal to the row count in a parent group (%d).';
  sdxInconsistencyKeyFoundTwice = 'Key ''%s'' found twice at indices ''%s'' and ''%d''';
  sdxInconsistencyWrongRowCountOfKeys = 'The number of returned rows (%d) does not equal the number of row keys in the query (%d)';
  sdxInconsistencyCantFindKeyInCompletelyFetchedGroup = 'Can''t find key ''%s'' in the completely fetched group';
  sdxInconsistencyCantFindKeyInNewlyFetchedGroup = 'Can''t find key ''%s'' in the newly fetched group';
  sdxInconsistencyUnexpectedNumberOfRows = 'Unexpected number of rows returned: %d. Expected: %d';
  sdxInconsistencyWrongKeyByRowIndex = 'Key ''%s'' of the row fetched at index %d does not match previously fetched key ''%s'' for the same index';
  sdxInconsistencyCantFindAppropriateGroupForKey = 'Can''t find appropriate group for row with key ''%s''';
  sdxInconsistencyCantFindAppropriateGroupForRow = 'Can''t find an appropriate group for row %d';
  sdxInconsistencyUnexpectedNumberOfKeys = 'Unexpected number of returned keys: %d. Expected: %d';
  sdxInconsistencyWrongKeyByIndex = 'Key ''%s'' fetched at index %d does not match previously fetched key ''%s'' for the same index';
  sdxInconsistencyWrongFetchedRow = 'Row with key ''%s'' fetched which was not queried. May be internal error or unsupported key design';

implementation

end.
