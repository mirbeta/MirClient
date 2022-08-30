{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxRichEdit.DocumentModel.Fields.TocEntryField;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Generics.Defaults, Generics.Collections,
  dxCoreClasses,

  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.MailMerge;

type
  { TdxTocEntryField }

  TdxTocEntryField = class(TdxCalculatedFieldBase)
  public const
    FieldType = 'TC';
  strict private
    class var
      FSwitchesWithArgument: TdxStringBooleanDictionary;
    class constructor Initialize;
    class destructor Finalize;
  strict private
    FText: string;
    FId: TdxNullableString;
    FLevel: Integer;
    FOmitPageNumber: Boolean;
  protected
    function GetSwitchesWithArguments: TdxStringBooleanDictionary; override;
    function GetFieldTypeName: string; override;
    function MailMergeType: TdxFieldMailMergeType; override;
  public
    class function CreateField: TdxCalculatedFieldBase; override;
    procedure Initialize(APieceTable: TdxCustomPieceTable; AInstructions: TdxInstructionCollection); override;
    function GetCalculatedValueCore(ASourcePieceTable: TdxCustomPieceTable;
      AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue; override;

    property Text: string read FText;
    property Id: TdxNullableString read FId;
    property Level: Integer read FLevel;
    property OmitPageNumber: Boolean read FOmitPageNumber;
  end;

implementation

{ TdxTocEntryField }

class constructor TdxTocEntryField.Initialize;
begin
  FSwitchesWithArgument := CreateSwitchesWithArgument(TArray<string>.Create('f', 'l'));
end;

class destructor TdxTocEntryField.Finalize;
begin
  FreeAndNil(FSwitchesWithArgument);
end;

class function TdxTocEntryField.CreateField: TdxCalculatedFieldBase;
begin
  Result := TdxTocEntryField.Create;
end;

function TdxTocEntryField.GetSwitchesWithArguments: TdxStringBooleanDictionary;
begin
  Result := FSwitchesWithArgument;
end;

function TdxTocEntryField.GetFieldTypeName: string;
begin
  Result := FieldType;
end;

procedure TdxTocEntryField.Initialize(APieceTable: TdxCustomPieceTable; AInstructions: TdxInstructionCollection);
var
  ALevelString: TdxNullableString;
begin
  inherited Initialize(APieceTable, AInstructions);

  FText := AInstructions.GetArgumentAsString(0);
  FId := AInstructions.GetString('f');
  FOmitPageNumber := AInstructions.GetBool('n');
  ALevelString := AInstructions.GetString('l');
  if ALevelString.IsNull then
    ALevelString := '1';
  if not TryStrToInt(ALevelString.Value, FLevel) then
    FLevel := 0;
end;

function TdxTocEntryField.MailMergeType: TdxFieldMailMergeType;
begin
  Result := TdxFieldMailMergeType.NonMailMerge;
end;

function TdxTocEntryField.GetCalculatedValueCore(ASourcePieceTable: TdxCustomPieceTable;
  AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue;
begin
  Result := TdxCalculatedFieldValue.Create('');
end;

end.
