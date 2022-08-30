{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
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

unit cxEditorFieldLinkReg;

{$I cxVer.inc}

interface

implementation

uses
  DB, Controls, cxDBEdit, cxEdit, cxEditReg;

const
  cxDefaultDbEditorsByFieldClasses: array[TFieldType] of TClass = (
    nil,                                // ftUnknown
    TcxDBTextEdit,                      // ftString
    TcxDBSpinEdit,                      // ftSmallint
    TcxDBSpinEdit,                      // ftInteger
    TcxDBSpinEdit,                      // ftWord
    TcxDBCheckBox,                      // ftBoolean
    TcxDBCalcEdit,                      // ftFloat
    TcxDBCurrencyEdit,                  // ftCurrency
    TcxDBCurrencyEdit,                  // ftBCD
    TcxDBDateEdit,                      // ftDate
    TcxDBTimeEdit,                      // ftTime
    TcxDBDateEdit,                      // ftDateTime
    TcxDBBlobEdit,                      // ftBytes
    TcxDBBlobEdit,                      // ftVarBytes
    TcxDBSpinEdit,                      // ftAutoInc
    TcxDBBlobEdit,                      // ftBlob
    TcxDBMemo,                          // ftMemo
    TcxDBImage,                         // ftGraphic
    TcxDBBlobEdit,                      // ftFmtMemo
    TcxDBBlobEdit,                      // ftParadoxOle
    TcxDBBlobEdit,                      // ftDBaseOle
    TcxDBBlobEdit,                      // ftTypedBinary
    nil,                                // ftCursor
    nil,                                // ftFixedChar
    TcxDBTextEdit,                      // ftWideString
    TcxDBSpinEdit,                      // ftLargeint
    TcxDBTextEdit,                      // ftADT
    TcxDBTextEdit,                      // ftArray
    nil,                                // ftReference
    nil,                                // ftDataSet
    nil,                                // ftOraBlob
    TcxDBMemo,                          // ftOraClob
    nil,                                // ftVariant
    nil,                                // ftInterface
    nil,                                // ftIDispatch
    nil,                                // ftGuid
    TcxDBDateEdit,                      // ftTimeStamp
    TcxDBTextEdit                       // ftFMTBcd
    , nil,                              // ftFixedWideChar
    TcxDBMemo,                          // ftWideMemo
    nil,                                // ftOraTimeStamp
    nil                                 // ftOraInterval
    , TcxDBSpinEdit,                    // ftLongWord
    TcxDBSpinEdit,                      // ftShortint
    nil,                                // ftByte
    nil,                                // ftExtended
    nil,                                // ftConnection
    nil,                                // ftParams
    nil                                 // ftStream
    , nil,                              // ftTimeStampOffset
    nil,                                // ftObject,
    nil                                 // ftSingle
    );

procedure GetControlClassName(AField: TField; out AClassName: string);
var
  AEditorClass: TClass;
begin
  AEditorClass := cxDefaultDbEditorsByFieldClasses[AField.DataType];
  if AEditorClass = nil then
    AClassName := ''
  else
    AClassName := AEditorClass.ClassName;
end;

initialization
  cxGetControlClassNameProc := GetControlClassName;

finalization
  cxGetControlClassNameProc := nil;

end.
