{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEntityMapping Framework                           }
{                                                                    }
{           Copyright (c) 2016-2019 Developer Express Inc.           }
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
{   (DCU, OBJ, DLL, DPU, SO, ETC.) ARE CONFIDENTIAL AND PROPRIETARY  }
{   TRADE SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER }
{   IS LICENSED TO DISTRIBUTE THE EXPRESSENTITYMAPPING FRAMEWORK     }
{   AS PART OF AN EXECUTABLE PROGRAM ONLY.                           }
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

unit dxEMF.Utils.Exceptions;

interface

{$I cxVer.inc}
{$I dxEMF.inc}

uses
  SysUtils,
  dxCore;

type

   { EdxEMFException }

   EdxEMFException = class(EdxException);

   { EdxNoEntityInfoException }

   EdxNoEntityInfoException = class(EdxEMFException);

   { EdxBaseEntityNoDiscriminatorColumnException }

   EdxBaseEntityNoDiscriminatorColumnException = class(EdxEMFException);

   { KeyPropertyAbsentException }

   EdxKeyPropertyAbsentException = class(EdxEMFException);

   { EdxNoSessionConnection }

   EdxNoSessionConnection = class(EdxEMFException);

  { EdxSchemaCorrectionNeededException }

  EdxSchemaCorrectionNeededException = class(EdxEMFException)
  strict private
    FSQL: string;
  public
    constructor Create(const ACommandText: string); overload;
    property SQL: string read FSQL;
  end;

  { EdxUnableToCreateDBObjectException }

  EdxUnableToCreateDBObjectException = class(EdxEMFException)
  strict private
    FObjectTypeName: string;
    FObjectName: string;
    FParentObjectName: string;
  public
    constructor Create(const AMsg: string); overload;
    constructor Create(const AObjectTypeName, AObjectName, AParentObjectName, AErrorMessage: string); overload;

    property ObjectTypeName: string read FObjectTypeName write FObjectTypeName;
    property ObjectName: string read FObjectName write FObjectName;
    property ParentObjectName: string read FParentObjectName write FParentObjectName;
  end;

  { EdxFormatException }

  EdxFormatException = class(EdxEMFException);

  { EdxUnknownProviderSQL }

  EdxUnknownProviderSQL = class(EdxEMFException);

  { EdxUnableToOpenDatabaseException }

  EdxUnableToOpenDatabaseException = class(EdxEMFException);

  { EdxInvalidPropertyPathException }

  EdxInvalidPropertyPathException = class(EdxEMFException);

  { EdxLockingException }

  EdxLockingException = class(EdxEMFException);

  { EdxAssociationInvalidException }

  EdxAssociationInvalidException = class(EdxEMFException);

  { EdxCannotLoadObjectsException }

  EdxCannotLoadObjectsException = class(EdxEMFException);

  { EdxCannotResolveClassInfoException }

  EdxCannotResolveClassInfoException = class(EdxEMFException);

  { EdxCriteriaParserException }

  EdxCriteriaParserException = class(EdxEMFException);

  { EdxNonPersistentReferenceFoundException }

  EdxNonPersistentReferenceFoundException = class(EdxEMFException);

  { EdxDifferentObjectsWithSameKeyException }

  EdxDifferentObjectsWithSameKeyException = class(EdxEMFException);

  { EdxTransactionSequenceException }

  EdxTransactionSequenceException = class(EdxEMFException);

  { EdxKeysAutogenerationNonSupportedTypeException }

  EdxKeysAutogenerationNonSupportedTypeException = class(EdxEMFException);

  { EdxDuplicateInheritedMemberNameFoundException }

  EdxDuplicateInheritedMemberNameFoundException = class(EdxEMFException);

implementation

uses
  dxEMF.Strs;

{ TdxSchemaCorrectionNeededException }

constructor EdxSchemaCorrectionNeededException.Create(const ACommandText: string);
begin
  inherited Create(ACommandText);
  FSQL := ACommandText;
end;

{ TdxUnableToCreateDBObjectException }

constructor EdxUnableToCreateDBObjectException.Create(const AObjectTypeName, AObjectName, AParentObjectName, AErrorMessage: string);
begin
  inherited CreateFmt(sdxConnectionProviderUnableToCreateDBObject, [AObjectTypeName, AObjectName, AParentObjectName, AErrorMessage]);
  FObjectTypeName := AObjectTypeName;
  FObjectName := AObjectName;
  FParentObjectName := AParentObjectName;
end;

constructor EdxUnableToCreateDBObjectException.Create(const AMsg: string);
begin
  inherited Create(AMsg);
end;

end.
