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

unit dxRichEdit.Import.Rtf.DestinationField;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, Windows, SysUtils, Generics.Defaults, Generics.Collections,
  dxCoreClasses,
  dxRichEdit.Import.Rtf,
  dxRichEdit.Import.Rtf.DestinationDefault,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.UnitConverter;

type
  { TdxFieldSubDestination }

  TdxFieldSubDestination = class abstract(TdxDefaultDestination)
  strict private
    FNestedGroupLevel: Integer;
  protected
    procedure OnDestinationClose; virtual;
    procedure EnsureFieldCreated(AFieldInfo: TdxRtfFieldInfo); virtual;
    function CreateField: TdxField;
    function CreateInstance: TdxFieldSubDestination; virtual; abstract;

    property NestedGroupLevel: Integer read FNestedGroupLevel;
  public
    constructor Create(AImporter: TdxRtfImporter); reintroduce;
    function CreateClone: TdxRichEditRtfDestinationBase; override;
    procedure BeforePopRtfState; override;
    procedure IncreaseGroupLevel; override;
  end;

  { TdxFieldDestination }

  TdxFieldDestination = class(TdxFieldSubDestination)
  strict private
    class var
      FKeywordHT: TdxKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateFieldKeywords: TdxKeywordTranslatorTable; static;
  strict private
    class function CreateFieldCodeDestination(AImporter: TdxRtfImporter): TdxRichEditRtfDestinationBase; static;
    class function CreateFieldResultDestination(AImporter: TdxRtfImporter): TdxRichEditRtfDestinationBase; static;

    class procedure FieldInstructionStartKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure FieldResultStartKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure FieldLockKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure FieldCodeViewKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure FieldEditKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure FieldDirtyKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
    class procedure FieldPrivateKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); static;
  protected
    class function GetKeywordHT: TdxKeywordTranslatorTable; override;
    class function GetControlCharHT: TdxControlCharTranslatorTable; override;
    procedure StartNewField; override;
    procedure OnDestinationClose; override;
    procedure ProcessField(AFieldInfo: TdxRtfFieldInfo);
    function CreateInstance: TdxFieldSubDestination; override;
  end;

  { TdxCodeFieldDestination }

  TdxCodeFieldDestination = class(TdxFieldSubDestination)
  protected
    function CreateInstance: TdxFieldSubDestination; override;
    procedure OnDestinationClose; override;
    procedure ProcessTextCore(const AText: string); override;
    procedure ProcessCharCore(ACh: Char); override;
    procedure StartNewField; override;
  end;

  { TdxCodeFieldHyperlinksDisabledDestination }

  TdxCodeFieldHyperlinksDisabledDestination = class(TdxCodeFieldDestination)
  protected
    function CreateInstance: TdxFieldSubDestination; override;
    procedure OnDestinationClose; override;
    procedure ProcessTextCore(const AText: string); override;
    procedure ProcessCharCore(ACh: Char); override;
    function DetectHyperlink(const AText: string): Boolean;
  end;

  { TdxResultFieldDestination }

  TdxResultFieldDestination = class(TdxFieldSubDestination)
  protected
    function CreateInstance: TdxFieldSubDestination; override;
    procedure OnDestinationClose; override;
  end;

implementation

uses
  StrUtils, Contnrs,
  dxStringHelper,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.History.FieldHistory;

{ TdxFieldSubDestination }

constructor TdxFieldSubDestination.Create(AImporter: TdxRtfImporter);
begin
  inherited Create(AImporter, AImporter.PieceTable);
  FNestedGroupLevel := 1;
end;

function TdxFieldSubDestination.CreateClone: TdxRichEditRtfDestinationBase;
var
  AClone: TdxFieldSubDestination;
begin
  if Self = nil then
    Exit(nil);
  AClone := CreateInstance;
  AClone.FNestedGroupLevel := FNestedGroupLevel;
  Result := AClone;
end;

procedure TdxFieldSubDestination.BeforePopRtfState;
begin
  inherited BeforePopRtfState;
  Dec(FNestedGroupLevel);
  if FNestedGroupLevel = 0 then
    OnDestinationClose;
end;

procedure TdxFieldSubDestination.IncreaseGroupLevel;
begin
  inherited IncreaseGroupLevel;
  Inc(FNestedGroupLevel);
end;

procedure TdxFieldSubDestination.OnDestinationClose;
begin
end;

procedure TdxFieldSubDestination.EnsureFieldCreated(AFieldInfo: TdxRtfFieldInfo);
var
  ALogPosition: TdxDocumentLogPosition;
begin
  if AFieldInfo.Field <> nil then
    Exit;
  ALogPosition := Importer.Position.LogPosition;
  AFieldInfo.Field := CreateField;
  Importer.Position.LogPosition := ALogPosition + 1;
end;

function TdxFieldSubDestination.CreateField: TdxField;
var
  AItem: TdxAddFieldHistoryItem;
begin
  AItem := TdxAddFieldHistoryItem.Create(PieceTable);
  try
    AItem.CodeStartRunIndex := PieceTable.InsertFieldCodeStartRunCore(Importer.Position);
    AItem.CodeEndRunIndex := PieceTable.InsertFieldCodeEndRunCore(Importer.Position);
    AItem.ResultEndRunIndex := PieceTable.InsertFieldResultEndRunCore(Importer.Position);
    AItem.Execute;
    Result := PieceTable.Fields[AItem.InsertedFieldIndex];
  finally
    AItem.Free;
  end;
end;

{ TdxFieldDestination }

class constructor TdxFieldDestination.Initialize;
begin
  FKeywordHT := CreateFieldKeywords;
end;

class destructor TdxFieldDestination.Finalize;
begin
  FreeAndNil(FKeywordHT);
end;

class function TdxFieldDestination.CreateFieldKeywords: TdxKeywordTranslatorTable;
begin
  Result := TdxKeywordTranslatorTable.Create;
  Result.Add('fldinst', FieldInstructionStartKeywordHandler);
  Result.Add('fldrslt', FieldResultStartKeywordHandler);
  Result.Add('fldlock', FieldLockKeywordHandler);
  Result.Add('fldedit', FieldEditKeywordHandler);
  Result.Add('flddirty', FieldDirtyKeywordHandler);
  Result.Add('fldpriv', FieldPrivateKeywordHandler);
  Result.Add('dxfldcodeview', FieldCodeViewKeywordHandler);
end;

class procedure TdxFieldDestination.FieldInstructionStartKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  ADestination: TdxFieldDestination;
  AFieldInfo: TdxRtfFieldInfo;
begin
  ADestination := TdxFieldDestination(AImporter.Destination);
  if ADestination.NestedGroupLevel <= 1 then
    AImporter.ThrowInvalidRtfFile;
  AFieldInfo := AImporter.Fields.Peek;
  if AFieldInfo.Field <> nil then
    AImporter.Position.LogPosition := AImporter.Position.LogPosition - 1;
  AImporter.Destination := CreateFieldCodeDestination(AImporter);
end;

class function TdxFieldDestination.CreateFieldCodeDestination(AImporter: TdxRtfImporter): TdxRichEditRtfDestinationBase;
begin
  if AImporter.DocumentModel.DocumentCapabilities.HyperlinksAllowed then
    Result := TdxCodeFieldDestination.Create(AImporter)
  else
    Result := TdxCodeFieldHyperlinksDisabledDestination.Create(AImporter);
end;

class procedure TdxFieldDestination.FieldResultStartKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  ADestination: TdxFieldDestination;
begin
  ADestination := TdxFieldDestination(AImporter.Destination);
  if ADestination.NestedGroupLevel <= 1 then
    AImporter.ThrowInvalidRtfFile;
  AImporter.Destination := CreateFieldResultDestination(AImporter);
end;

class function TdxFieldDestination.CreateFieldResultDestination(AImporter: TdxRtfImporter): TdxRichEditRtfDestinationBase;
var
  AFieldInfo: TdxRtfFieldInfo;
begin
  AFieldInfo := AImporter.Fields.Peek;
  if not AImporter.DocumentModel.DocumentCapabilities.HyperlinksAllowed and AFieldInfo.IsHyperlink then
    Result := TdxResultFieldDestination.Create(AImporter)
  else
  begin
    if AFieldInfo.Field = nil then
      Result := CreateFieldCodeDestination(AImporter)
    else
      Result := TdxResultFieldDestination.Create(AImporter);
  end;
end;

class procedure TdxFieldDestination.FieldLockKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  AImporter.Fields.Peek.IsLock := True;
end;

class procedure TdxFieldDestination.FieldCodeViewKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
  if not AImporter.DocumentModel.FieldOptions.UpdateFieldsOnPaste then
    AImporter.Fields.Peek.IsCodeView := True;
end;

class procedure TdxFieldDestination.FieldEditKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
end;

class procedure TdxFieldDestination.FieldDirtyKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
end;

class procedure TdxFieldDestination.FieldPrivateKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
end;

class function TdxFieldDestination.GetKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := FKeywordHT;
end;

class function TdxFieldDestination.GetControlCharHT: TdxControlCharTranslatorTable;
begin
  Result := nil;
end;

procedure TdxFieldDestination.StartNewField;
begin
end;

procedure TdxFieldDestination.OnDestinationClose;
begin
  ProcessField(Importer.Fields.Peek);
  Importer.Fields.Pop;
end;

procedure TdxFieldDestination.ProcessField(AFieldInfo: TdxRtfFieldInfo);
begin
  if AFieldInfo.Field = nil then
  begin
    if AFieldInfo.IsHyperlink then
      Exit;
    AFieldInfo.Field := CreateField;
  end
  else
    Importer.Position.LogPosition := Importer.Position.LogPosition + 1;

  AFieldInfo.Field.Locked := AFieldInfo.IsLock;
  AFieldInfo.Field.IsCodeView := AFieldInfo.IsCodeView;
end;

function TdxFieldDestination.CreateInstance: TdxFieldSubDestination;
begin
  Result := TdxFieldDestination.Create(Importer);
end;

{ TdxCodeFieldDestination }

function TdxCodeFieldDestination.CreateInstance: TdxFieldSubDestination;
begin
  Result := TdxCodeFieldDestination.Create(Importer);
end;

procedure TdxCodeFieldDestination.OnDestinationClose;
begin
  EnsureFieldCreated(Importer.Fields.Peek);
  Importer.Position.LogPosition := Importer.Position.LogPosition + 1;
end;

procedure TdxCodeFieldDestination.ProcessTextCore(const AText: string);
begin
  EnsureFieldCreated(Importer.Fields.Peek);
  inherited ProcessTextCore(AText);
end;

procedure TdxCodeFieldDestination.ProcessCharCore(ACh: Char);
begin
  EnsureFieldCreated(Importer.Fields.Peek);
  inherited ProcessCharCore(ACh);
end;

procedure TdxCodeFieldDestination.StartNewField;
begin
  EnsureFieldCreated(Importer.Fields.Peek);
  inherited StartNewField;
end;

{ TdxCodeFieldHyperlinksDisabledDestination }

function TdxCodeFieldHyperlinksDisabledDestination.CreateInstance: TdxFieldSubDestination;
begin
  Result := TdxCodeFieldHyperlinksDisabledDestination.Create(Importer);
end;

procedure TdxCodeFieldHyperlinksDisabledDestination.OnDestinationClose;
var
  AFieldInfo: TdxRtfFieldInfo;
begin
  AFieldInfo := Importer.Fields.Peek;
  if AFieldInfo.IsHyperlink then
    Exit;

  inherited OnDestinationClose;
end;

procedure TdxCodeFieldHyperlinksDisabledDestination.ProcessTextCore(const AText: string);
var
  AFieldInfo: TdxRtfFieldInfo;
begin
  AFieldInfo := Importer.Fields.Peek;
  if AFieldInfo.IsHyperlink then
    Exit;

  if DetectHyperlink(AText) then
  begin
    AFieldInfo.IsHyperlink := True;
    Exit;
  end;

  inherited ProcessTextCore(AText);
end;

procedure TdxCodeFieldHyperlinksDisabledDestination.ProcessCharCore(ACh: Char);
var
  AFieldInfo: TdxRtfFieldInfo;
begin
  AFieldInfo := Importer.Fields.Peek;
  if AFieldInfo.IsHyperlink then
    Exit;
  inherited ProcessCharCore(ACh);
end;

function TdxCodeFieldHyperlinksDisabledDestination.DetectHyperlink(const AText: string): Boolean;
begin
  Result := TdxStringHelper.IndexOf(UpperCase(AText), 'HYPERLINK') >= 0;
end;

{ TdxResultFieldDestination }

function TdxResultFieldDestination.CreateInstance: TdxFieldSubDestination;
begin
  Result := TdxResultFieldDestination.Create(Importer);
end;

procedure TdxResultFieldDestination.OnDestinationClose;
begin
end;

end.
