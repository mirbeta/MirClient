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
{   LICENSED TO DISTRIBUTE THE EXPRESSDATACONTROLLER AND ALL         }
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

unit cxDataConsts;

{$I cxVer.inc}

interface

resourcestring
  // Data
  cxSDataReadError = 'Stream read error';
  cxSDataWriteError = 'Stream write error';
  cxSDataItemExistError = 'Item already exists';
  cxSDataRecordIndexError = 'RecordIndex out of range';
  cxSDataItemIndexError = 'ItemIndex out of range';
  cxSDataProviderModeError = 'This operation is not supported in provider mode';
  cxSDataInvalidStreamFormat = 'Invalid stream format';
  cxSDataRowIndexError = 'RowIndex out of range';
//  cxSDataRelationItemExistError = 'Relation Item already exists';
//  cxSDataRelationCircularReference = 'Circular Reference on Detail DataController';
//  cxSDataRelationMultiReferenceError = 'Reference on Detail DataController already exists';
  cxSDataCustomDataSourceInvalidCompare = 'GetInfoForCompare not implemented';
  // DB
//  cxSDBDataSetNil = 'DataSet is nil';
  cxSDBDetailFilterControllerNotFound = 'DetailFilterController not found';
  cxSDBNotInGridMode = 'DataController not in GridMode';
  cxSDBKeyFieldNotFound = 'Key Field not found';

implementation

uses
  dxCore;

procedure AddExpressDataControllerResourceStringNames(AProduct: TdxProductResourceStrings);

  procedure InternalAdd(const AResourceStringName: string; AAddress: Pointer);
  begin
    AProduct.Add(AResourceStringName, AAddress);
  end;

begin
  InternalAdd('cxSDataReadError', @cxSDataReadError);
  InternalAdd('cxSDataWriteError', @cxSDataWriteError);
  InternalAdd('cxSDataItemExistError', @cxSDataItemExistError);
  InternalAdd('cxSDataRecordIndexError', @cxSDataRecordIndexError);
  InternalAdd('cxSDataItemIndexError', @cxSDataItemIndexError);
  InternalAdd('cxSDataProviderModeError', @cxSDataProviderModeError);
  InternalAdd('cxSDataInvalidStreamFormat', @cxSDataInvalidStreamFormat);
  InternalAdd('cxSDataRowIndexError', @cxSDataRowIndexError);
  InternalAdd('cxSDataCustomDataSourceInvalidCompare', @cxSDataCustomDataSourceInvalidCompare);
  InternalAdd('cxSDBDetailFilterControllerNotFound', @cxSDBDetailFilterControllerNotFound);
  InternalAdd('cxSDBNotInGridMode', @cxSDBNotInGridMode);
  InternalAdd('cxSDBKeyFieldNotFound', @cxSDBKeyFieldNotFound);
end;

initialization
  dxResourceStringsRepository.RegisterProduct('ExpressDataController', @AddExpressDataControllerResourceStringNames);

finalization
  dxResourceStringsRepository.UnRegisterProduct('ExpressDataController');

end.
