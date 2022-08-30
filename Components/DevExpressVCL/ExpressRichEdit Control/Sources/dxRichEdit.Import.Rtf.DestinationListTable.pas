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

unit dxRichEdit.Import.Rtf.DestinationListTable;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, Classes, SysUtils, Contnrs, Graphics, Windows, Generics.Defaults, Generics.Collections,
  dxCoreClasses,
  dxRichEdit.Import.Rtf,
  dxRichEdit.Import.Rtf.DestinationPieceTable,
  dxRichEdit.Import.Rtf.DestinationListLevel;

type
  { TdxListNameDestination }

  TdxListNameDestination = class(TdxStringValueDestination)
  protected
    function CreateEmptyClone: TdxStringValueDestination; override;
  end;

  { TdxListStyleNameDestination }

  TdxListStyleNameDestination = class(TdxStringValueDestination)
  protected
    function CreateEmptyClone: TdxStringValueDestination; override;
  end;

  { TdxListTableDestination }

  TdxListTableDestination = class(TdxRichEditRtfDestinationBase)
  strict private
    class var FKeywordHT: TdxKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateKeywordHT: TdxKeywordTranslatorTable; static;
  private
    FCurrentList: TdxRtfNumberingList;
    procedure SetCurrentList(Value: TdxRtfNumberingList);
    procedure TryToHandleFinishOfListLevelDestination(ANestedDestination: TdxRichEditRtfDestinationBase);
    procedure TryToHandleFinishOfListNameDestination(ANestedDestination: TdxRichEditRtfDestinationBase);
    procedure TryToHandleFinishOfListStyleNameDestination(ANestedDestination: TdxRichEditRtfDestinationBase);
    class procedure ListHybridKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ListIdKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ListKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ListLevelKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ListNameKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ListRestartAtEachSectionKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ListSimpleKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ListStyleIdKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ListStyleNameKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ListTemplateIdKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
  protected
    function CanProcessDefaultKeyword: Boolean; override;
    function CreateClone: TdxRichEditRtfDestinationBase; override;
    class function GetKeywordHT: TdxKeywordTranslatorTable; override;
    procedure ProcessCharCore(AChar: Char); override;
  public
    constructor Create(AImporter: TdxRtfImporter); override;
    destructor Destroy; override;

    procedure NestedGroupFinished(ADestination: TdxRichEditRtfDestinationBase); override;

    property CurrentList: TdxRtfNumberingList read FCurrentList;
  end;

  { TdxListOverrideTableDestination }

  TdxListOverrideTableDestination = class(TdxRichEditRtfDestinationBase)
  strict private
    class var FKeywordHT: TdxKeywordTranslatorTable;
    class constructor Initialize;
    class destructor Finalize;
    class function CreateKeywordHT: TdxKeywordTranslatorTable; static;
  private
    FCurrentOverride: TdxRtfNumberingListOverride;
    class procedure ListOverrideCountKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ListOverrideIdKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ListOverrideKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ListOverrideLevelKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
    class procedure ListOverrideListIdKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean); inline; static;
  protected
    function CreateClone: TdxRichEditRtfDestinationBase; override;
    class function GetKeywordHT: TdxKeywordTranslatorTable; override;

    property CurrentOverride: TdxRtfNumberingListOverride read FCurrentOverride write FCurrentOverride;
  end;

implementation

uses
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.Import.Rtf.DestinationSkip;

{ TdxListNameDestination }

function TdxListNameDestination.CreateEmptyClone: TdxStringValueDestination;
begin
  Result := TdxListNameDestination.Create(Importer);
end;

{ TdxListStyleNameDestination }

function TdxListStyleNameDestination.CreateEmptyClone: TdxStringValueDestination;
begin
  Result := TdxListStyleNameDestination.Create(Importer);
end;

{ TdxListTableDestination }

constructor TdxListTableDestination.Create(AImporter: TdxRtfImporter);
begin
  inherited Create(AImporter);
end;

destructor TdxListTableDestination.Destroy;
begin
  inherited Destroy;
end;

function TdxListTableDestination.CreateClone: TdxRichEditRtfDestinationBase;
begin
  Result := TdxListTableDestination.Create(Importer);
  TdxListTableDestination(Result).SetCurrentList(CurrentList);
end;

class constructor TdxListTableDestination.Initialize;
begin
  FKeywordHT := CreateKeywordHT;
end;

class destructor TdxListTableDestination.Finalize;
begin
  FreeAndNil(FKeywordHT);
end;

class function TdxListTableDestination.GetKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := FKeywordHT;
end;

class function TdxListTableDestination.CreateKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := TdxKeywordTranslatorTable.Create;
  Result.Add('list', ListKeywordHandler);
  Result.Add('listid', ListIdKeywordHandler);
  Result.Add('listtemplateid', ListTemplateIdKeywordHandler);
  Result.Add('liststyleid', ListStyleIdKeywordHandler);
  Result.Add('liststylename', ListStyleNameKeywordHandler);
  Result.Add('listname', ListNameKeywordHandler);
  Result.Add('listhybrid', ListHybridKeywordHandler);
  Result.Add('listrestarthdn', ListRestartAtEachSectionKeywordHandler);
  Result.Add('listsimple', ListSimpleKeywordHandler);
  Result.Add('listlevel', ListLevelKeywordHandler);
end;

procedure TdxListTableDestination.NestedGroupFinished(ADestination: TdxRichEditRtfDestinationBase);
begin
  TryToHandleFinishOfListNameDestination(ADestination);
  TryToHandleFinishOfListStyleNameDestination(ADestination);
  TryToHandleFinishOfListLevelDestination(ADestination);
end;

function TdxListTableDestination.CanProcessDefaultKeyword: Boolean;
begin
  Result := False;
end;

procedure TdxListTableDestination.ProcessCharCore(AChar: Char);
begin
//do nothing
end;

procedure TdxListTableDestination.SetCurrentList(Value: TdxRtfNumberingList);
begin
  FCurrentList.Free;
  FCurrentList := Value;
end;

procedure TdxListTableDestination.TryToHandleFinishOfListLevelDestination(ANestedDestination: TdxRichEditRtfDestinationBase);
var
  ACurrentDestination: TdxListTableDestination;
  ADestination: TdxListLevelDestination;
begin
  if ANestedDestination is TdxListLevelDestination then
  begin
    ACurrentDestination := TdxListTableDestination(Importer.Destination);
    ADestination := TdxListLevelDestination(ANestedDestination);
    Importer.RtfLevels.Extract(ADestination.Level);
    ACurrentDestination.CurrentList.Levels.Add(ADestination.Level);
  end;
end;

procedure TdxListTableDestination.TryToHandleFinishOfListNameDestination(ANestedDestination: TdxRichEditRtfDestinationBase);
var
  ACurrentDestination: TdxListTableDestination;
  ADestination: TdxListNameDestination;
begin
  if ANestedDestination is TdxListNameDestination then
  begin
    ACurrentDestination := TdxListTableDestination(Importer.Destination);
    ADestination := TdxListNameDestination(ANestedDestination);
    ACurrentDestination.CurrentList.Name := ADestination.Value;
  end;
end;

procedure TdxListTableDestination.TryToHandleFinishOfListStyleNameDestination(ANestedDestination: TdxRichEditRtfDestinationBase);
var
  ACurrentDestination: TdxListTableDestination;
  ADestination: TdxListStyleNameDestination;
begin
  if ANestedDestination is TdxListStyleNameDestination then
  begin
    ACurrentDestination := TdxListTableDestination(Importer.Destination);
    ADestination := TdxListStyleNameDestination(ANestedDestination);
    ACurrentDestination.CurrentList.StyleName := ADestination.Value;
  end;
end;

class procedure TdxListTableDestination.ListHybridKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
var
  ADestination: TdxListTableDestination;
begin
  ADestination := TdxListTableDestination(AImporter.Destination);
  ADestination.CurrentList.NumberingListType := TdxRtfNumberingListType.Hybrid;
end;

class procedure TdxListTableDestination.ListIdKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
var
  ADestination: TdxListTableDestination;
begin
  if AHasParameter then
  begin
    ADestination := TdxListTableDestination(AImporter.Destination);
    ADestination.CurrentList.ID := AParameterValue;
  end;
end;

class procedure TdxListTableDestination.ListKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
var
  ADestination: TdxListTableDestination;
begin
  ADestination := TdxListTableDestination(AImporter.Destination);
  ADestination.SetCurrentList(TdxRtfNumberingList.Create);
  AImporter.DocumentProperties.ListTable.Add(ADestination.CurrentList);
end;

class procedure TdxListTableDestination.ListLevelKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
var
  ADestination: TdxListLevelDestination;
begin
  ADestination := TdxListLevelDestination.Create(AImporter);
  AImporter.Position.CharacterFormatting.BeginUpdate;
  AImporter.Position.CharacterFormatting.ResetUse(TdxCharacterFormattingOptions.MaskUseAll);
  AImporter.Position.CharacterFormatting.EndUpdate;
  AImporter.Destination := ADestination;
end;

class procedure TdxListTableDestination.ListNameKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  AImporter.Destination := TdxListNameDestination.Create(AImporter);
end;

class procedure TdxListTableDestination.ListRestartAtEachSectionKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
//do nothing
end;

class procedure TdxListTableDestination.ListSimpleKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
//do nothing
end;

class procedure TdxListTableDestination.ListStyleIdKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
var
  ADestination: TdxListTableDestination;
begin
  if AHasParameter then
  begin
    ADestination := TdxListTableDestination(AImporter.Destination);
    ADestination.CurrentList.ParentStyleID := AParameterValue;
  end;
end;

class procedure TdxListTableDestination.ListStyleNameKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
  AImporter.Destination := TdxListStyleNameDestination.Create(AImporter);
end;

class procedure TdxListTableDestination.ListTemplateIdKeywordHandler(
  AImporter: TdxRtfImporter; AParameterValue: Integer;
  AHasParameter: Boolean);
begin
//do nothing
end;

{ TdxListOverrideTableDestination }

function TdxListOverrideTableDestination.CreateClone: TdxRichEditRtfDestinationBase;
begin
  Result := TdxListOverrideTableDestination.Create(Importer);
  TdxListOverrideTableDestination(Result).CurrentOverride := CurrentOverride;
end;

class function TdxListOverrideTableDestination.GetKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := FKeywordHT;
end;

class constructor TdxListOverrideTableDestination.Initialize;
begin
  FKeywordHT := CreateKeywordHT;
end;

class destructor TdxListOverrideTableDestination.Finalize;
begin
  FreeAndNil(FKeywordHT);
end;

class function TdxListOverrideTableDestination.CreateKeywordHT: TdxKeywordTranslatorTable;
begin
  Result := TdxKeywordTranslatorTable.Create;
  Result.Add('listoverride', ListOverrideKeywordHandler);
  Result.Add('listid', ListOverrideListIdKeywordHandler);
  Result.Add('listoverridecount', ListOverrideCountKeywordHandler);
  Result.Add('ls', ListOverrideIdKeywordHandler);
  Result.Add('lfolevel', ListOverrideLevelKeywordHandler);
end;

class procedure TdxListOverrideTableDestination.ListOverrideCountKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
begin
//do nothing
end;

class procedure TdxListOverrideTableDestination.ListOverrideIdKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  ADestination: TdxListOverrideTableDestination;
begin
  if AHasParameter then
  begin
    ADestination := TdxListOverrideTableDestination(AImporter.Destination);
    if (ADestination.CurrentOverride <> nil) then
      ADestination.CurrentOverride.ID := AParameterValue;
  end;
end;

class procedure TdxListOverrideTableDestination.ListOverrideKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  ADestination: TdxListOverrideTableDestination;
begin
  ADestination := TdxListOverrideTableDestination(AImporter.Destination);
  ADestination.CurrentOverride := TdxRtfNumberingListOverride.Create;
  AImporter.DocumentProperties.ListOverrideTable.Add(ADestination.CurrentOverride);
end;

class procedure TdxListOverrideTableDestination.ListOverrideLevelKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  ADestination: TdxListOverrideTableDestination;
  ANewDestination: TdxListOverrideLevelDestination;
begin
  ADestination := TdxListOverrideTableDestination(AImporter.Destination);
  ANewDestination := TdxListOverrideLevelDestination.Create(AImporter);
  AImporter.Destination := ANewDestination;
  if ADestination.CurrentOverride <> nil then
    ADestination.CurrentOverride.Levels.Add(ANewDestination.OverrideLevel);
end;

class procedure TdxListOverrideTableDestination.ListOverrideListIdKeywordHandler(AImporter: TdxRtfImporter; AParameterValue: Integer; AHasParameter: Boolean);
var
  ADestination: TdxListOverrideTableDestination;
begin
  if AHasParameter then
  begin
    ADestination := TdxListOverrideTableDestination(AImporter.Destination);
    if ADestination.CurrentOverride <> nil then
      ADestination.CurrentOverride.ListId := AParameterValue;
  end;
end;

end.
