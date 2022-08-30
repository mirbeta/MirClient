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

unit dxRichEdit.DocumentModel.Fields.HyperlinkField;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Generics.Defaults, Generics.Collections,
  dxCoreClasses,
  dxRichEdit.Options.Simple,
  dxRichEdit.Options,
  dxGenerics,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.MailMerge,
  dxRichEdit.DocumentModel.Hyperlink;

type
  { TdxHyperlinkField }

  TdxHyperlinkField = class(TdxCalculatedFieldBase)
  public const
    FieldType = 'HYPERLINK';
  strict private class var
    FSwitchesWithArgument: TdxStringBooleanDictionary;
  strict private
    FLocation: string;
    FLocationInFile: string;
    FAppendCoordinates: Boolean;
    FOpenInNewWindow: Boolean;
    FScreenTip: string;
    FTarget: string;
    class constructor Initialize;
    class destructor Finalize;
  protected
    function GetSwitchesWithArguments: TdxStringBooleanDictionary; override;
    function GetFieldTypeName: string; override;
    function CreateHyperlinkInfo: TdxHyperlinkInfo;
    procedure UpdateVisited(ASourcePieceTable: TdxCustomPieceTable; AInfo: TdxHyperlinkInfo);
    function GetHyperlinkInfo(ASourcePieceTable: TdxSimplePieceTable; AField: TdxField): TdxHyperlinkInfo; virtual;
  public
    class function CreateField: TdxCalculatedFieldBase; override;
    procedure Initialize(APieceTable: TdxCustomPieceTable; AInstructions: TdxInstructionCollection); override;
    function GetCalculatedValueCore(ASourcePieceTable: TdxCustomPieceTable;
      AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue; override;
    function GetAllowedUpdateFieldTypes(const AOptions: TdxFieldUpdateOnLoadOptions): TdxUpdateFieldOperationTypes; override;
    function Update(ASourcePieceTable: TdxCustomPieceTable; AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue; override;

    property Location: string read FLocation;
    property LocationInFile: string read FLocationInFile;
    property AppendCoordinates: Boolean read FAppendCoordinates;
    property OpenInNewWindow: Boolean read FOpenInNewWindow;
    property ScreenTip: string read FScreenTip;
    property Target: string read FTarget;
  end;

implementation

{ TdxHyperlinkField }

class constructor TdxHyperlinkField.Initialize;
begin
  FSwitchesWithArgument := CreateSwitchesWithArgument(TArray<string>.Create('l', 'o', 't'));
end;

class destructor TdxHyperlinkField.Finalize;
begin
  FreeAndNil(FSwitchesWithArgument);
end;

class function TdxHyperlinkField.CreateField: TdxCalculatedFieldBase;
begin
  Result := TdxHyperlinkField.Create;
end;

function TdxHyperlinkField.GetSwitchesWithArguments: TdxStringBooleanDictionary;
begin
  Result := FSwitchesWithArgument;
end;

function TdxHyperlinkField.GetFieldTypeName: string;
begin
  Result := FieldType;
end;

procedure TdxHyperlinkField.Initialize(APieceTable: TdxCustomPieceTable; AInstructions: TdxInstructionCollection);
begin
  inherited Initialize(APieceTable, AInstructions);
  FLocation := TdxHyperlinkUriHelper.ConvertFromHyperlinkUri(AInstructions.GetArgumentAsString(0));
  FLocationInFile := AInstructions.GetString('l').Value;
  FAppendCoordinates := AInstructions.GetBool('m');
  FOpenInNewWindow := AInstructions.GetBool('n');
  FScreenTip := AInstructions.GetString('o').Value;
  FTarget := AInstructions.GetString('t').Value;
end;

function TdxHyperlinkField.GetCalculatedValueCore(ASourcePieceTable: TdxCustomPieceTable; AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue;
begin
  Result := TdxCalculatedFieldValue.NullValue;
end;

function TdxHyperlinkField.Update(ASourcePieceTable: TdxCustomPieceTable; AMailMergeDataMode: TdxMailMergeDataMode; ADocumentField: TdxField): TdxCalculatedFieldValue;
var
  AInfo, AOldInfo: TdxHyperlinkInfo;
  AResult: string;
  APieceTable: TdxSimplePieceTable;
begin
  APieceTable := TdxSimplePieceTable(ASourcePieceTable);
  if not APieceTable.DocumentModel.DocumentCapabilities.HyperlinksAllowed then
    Exit(TdxCalculatedFieldValue.Create(nil, [TdxFieldResultOption.KeepOldResult]));
  AInfo := CreateHyperlinkInfo;
  UpdateVisited(APieceTable, AInfo);
  AOldInfo := GetHyperlinkInfo(APieceTable, ADocumentField);
  if AOldInfo <> nil then
    APieceTable.ReplaceHyperlinkInfo(ADocumentField.Index, AInfo)
  else
  begin
    APieceTable.InsertHyperlinkInfo(ADocumentField.Index, AInfo);
    if ADocumentField.Result.Start = ADocumentField.Result.&End then
    begin
      if FLocation <> '' then
        AResult := FLocation
      else
        AResult := FLocationInFile;
      if AResult <> '' then
        APieceTable.ModifyHyperlinkResult(ADocumentField, AResult);
    end;
  end;
  Result := TdxCalculatedFieldValue.Create(nil, [TdxFieldResultOption.HyperlinkField]);
end;

function TdxHyperlinkField.GetAllowedUpdateFieldTypes(const AOptions: TdxFieldUpdateOnLoadOptions): TdxUpdateFieldOperationTypes;
begin
  Result := [TdxUpdateFieldOperationType.Normal, TdxUpdateFieldOperationType.Load,
    TdxUpdateFieldOperationType.Copy, TdxUpdateFieldOperationType.PasteFromIE,
    TdxUpdateFieldOperationType.CreateModelForExport];
end;

function TdxHyperlinkField.CreateHyperlinkInfo: TdxHyperlinkInfo;
begin
  Result := TdxHyperlinkInfo.Create;
  Result.NavigateUri := FLocation;
  Result.Anchor := FLocationInFile;
  Result.Target := FTarget;
  Result.ToolTip := FScreenTip;
end;

procedure TdxHyperlinkField.UpdateVisited(ASourcePieceTable: TdxCustomPieceTable; AInfo: TdxHyperlinkInfo);
begin
  AInfo.Visited := False;
end;

function TdxHyperlinkField.GetHyperlinkInfo(ASourcePieceTable: TdxSimplePieceTable; AField: TdxField): TdxHyperlinkInfo;
var
  AHyperlinkInfos: TdxHyperlinkInfoCollection;
begin
  AHyperlinkInfos := ASourcePieceTable.HyperlinkInfos;
  if AHyperlinkInfos.IsHyperlink(AField.Index) then
    Result := AHyperlinkInfos[AField.Index]
  else
    Result := nil;
end;

end.
