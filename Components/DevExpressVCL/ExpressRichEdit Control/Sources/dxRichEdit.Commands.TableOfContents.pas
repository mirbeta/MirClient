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

unit dxRichEdit.Commands.TableOfContents;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils,

  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.View.Core,
  dxRichEdit.Commands,
  dxRichEdit.Commands.IDs,
  dxRichEdit.Commands.Insert,
  dxRichEdit.Commands.ChangeProperties,
  dxRichEdit.Commands.Fields,
  dxRichEdit.Commands.Selection;

type
  { TdxInsertTableOfContentsCoreBaseCommand }

  TdxInsertTableOfContentsCoreBaseCommand = class abstract(TdxInsertFieldCoreCommand)
  protected
    procedure UpdateField(AField: TdxField); override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  end;

  { TdxInsertTableOfContentsCoreCommand }

  TdxInsertTableOfContentsCoreCommand = class(TdxInsertTableOfContentsCoreBaseCommand)
  public
    constructor Create(const AControl: IdxRichEditControl); reintroduce; override;

    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertTableOfEquationsCoreCommand }

  TdxInsertTableOfEquationsCoreCommand = class(TdxInsertTableOfContentsCoreBaseCommand)
  public
    constructor Create(const AControl: IdxRichEditControl); reintroduce; override;

    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertTableOfFiguresCoreCommand }

  TdxInsertTableOfFiguresCoreCommand = class(TdxInsertTableOfContentsCoreBaseCommand)
  public
    constructor Create(const AControl: IdxRichEditControl); reintroduce; override;

    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertTableOfTablesCoreCommand }

  TdxInsertTableOfTablesCoreCommand = class(TdxInsertTableOfContentsCoreBaseCommand)
  public
    constructor Create(const AControl: IdxRichEditControl); reintroduce; override;

    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertTableOfFiguresPlaceholderCommand }

  TdxInsertTableOfFiguresPlaceholderCommand = class(TdxRichEditMenuItemSimpleCommand)
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertTableOfContentsCommand }

  TdxInsertTableOfContentsCommand = class(TdxTransactedInsertObjectCommand)
  protected
    class function GetInsertObjectCommandClass: TdxRichEditCommandClass; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxInsertTableOfEquationsCommand }

  TdxInsertTableOfEquationsCommand = class(TdxTransactedInsertObjectCommand)
  protected
    class function GetInsertObjectCommandClass: TdxRichEditCommandClass; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxInsertTableOfFiguresCommand }

  TdxInsertTableOfFiguresCommand = class(TdxTransactedInsertObjectCommand)
  protected
    class function GetInsertObjectCommandClass: TdxRichEditCommandClass; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxInsertTableOfTablesCommand }

  TdxInsertTableOfTablesCommand = class(TdxTransactedInsertObjectCommand)
  protected
    class function GetInsertObjectCommandClass: TdxRichEditCommandClass; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxChangeHeadingLevelCommandBase }

  TdxChangeHeadingLevelCommandBase = class abstract(TdxChangeParagraphStyleCommandBase)
  protected
    procedure ChangeParagraphProperty(AParagraph: TdxParagraph); override;
    function CalculateParagraphStyleIndex(AParagraph: TdxParagraph): Integer; override;
    function CalculateHeadingLevel(AParagraph: TdxParagraph): Integer; virtual; abstract;
    function CalculateOutlineLevel(AParagraph: TdxParagraph): Integer; virtual; abstract;
  end;

  { TdxIncrementParagraphOutlineLevelCommand }

  TdxIncrementParagraphOutlineLevelCommand = class(TdxChangeHeadingLevelCommandBase)
  protected
    function CalculateHeadingLevel(AParagraph: TdxParagraph): Integer; override;
    function CalculateOutlineLevel(AParagraph: TdxParagraph): Integer; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxDecrementParagraphOutlineLevelCommand }

  TdxDecrementParagraphOutlineLevelCommand = class(TdxChangeHeadingLevelCommandBase)
  protected
    function CalculateHeadingLevel(AParagraph: TdxParagraph): Integer; override;
    function CalculateOutlineLevel(AParagraph: TdxParagraph): Integer; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxSetParagraphHeadingLevelCommandBase }

  TdxSetParagraphHeadingLevelCommandBase = class abstract(TdxChangeHeadingLevelCommandBase)
  protected
    class function GetLevel: Integer; virtual; abstract;
    function CalculateHeadingLevel(AParagraph: TdxParagraph): Integer; override;
    function CalculateOutlineLevel(AParagraph: TdxParagraph): Integer; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxSetParagraphHeading1LevelCommand }

  TdxSetParagraphHeading1LevelCommand = class(TdxSetParagraphHeadingLevelCommandBase)
  protected
    class function GetLevel: Integer; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxSetParagraphHeading2LevelCommand }

  TdxSetParagraphHeading2LevelCommand = class(TdxSetParagraphHeadingLevelCommandBase)
  protected
    class function GetLevel: Integer; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxSetParagraphHeading3LevelCommand }

  TdxSetParagraphHeading3LevelCommand = class(TdxSetParagraphHeadingLevelCommandBase)
  protected
    class function GetLevel: Integer; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxSetParagraphHeading4LevelCommand }

  TdxSetParagraphHeading4LevelCommand = class(TdxSetParagraphHeadingLevelCommandBase)
  protected
    class function GetLevel: Integer; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxSetParagraphHeading5LevelCommand }

  TdxSetParagraphHeading5LevelCommand = class(TdxSetParagraphHeadingLevelCommandBase)
  protected
    class function GetLevel: Integer; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxSetParagraphHeading6LevelCommand }

  TdxSetParagraphHeading6LevelCommand = class(TdxSetParagraphHeadingLevelCommandBase)
  protected
    class function GetLevel: Integer; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxSetParagraphHeading7LevelCommand }

  TdxSetParagraphHeading7LevelCommand = class(TdxSetParagraphHeadingLevelCommandBase)
  protected
    class function GetLevel: Integer; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxSetParagraphHeading8LevelCommand }

  TdxSetParagraphHeading8LevelCommand = class(TdxSetParagraphHeadingLevelCommandBase)
  protected
    class function GetLevel: Integer; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxSetParagraphHeading9LevelCommand }

  TdxSetParagraphHeading9LevelCommand = class(TdxSetParagraphHeadingLevelCommandBase)
  protected
    class function GetLevel: Integer; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxSetParagraphBodyTextLevelCommand }

  TdxSetParagraphBodyTextLevelCommand = class(TdxSetParagraphHeadingLevelCommandBase)
  protected
    class function GetLevel: Integer; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxAddParagraphsToTableOfContentsCommand }

  TdxAddParagraphsToTableOfContentsCommand = class(TdxRichEditMenuItemSimpleCommand)
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxUpdateTableOfContentsCommand }

  TdxUpdateTableOfContentsCommand = class(TdxFieldBasedRichEditMenuItemSimpleCommand)
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    constructor Create(const AControl: IdxRichEditControl); reintroduce; override;
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetImageName: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxUpdateTableOfFiguresCommand }

  TdxUpdateTableOfFiguresCommand = class(TdxUpdateTableOfContentsCommand)
  protected
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertCaptionCommandBase }

  TdxInsertCaptionCommandBase = class abstract(TdxTransactedInsertObjectCommand)
  protected
    function GetInsertObjectCommand: TdxRichEditCommand; override;
    function GetPrefix: string; virtual; abstract;
    procedure CreateCommands; override;

    property Prefix: string read GetPrefix;
  end;

  { TdxInsertEquationCaptionCommand }

  TdxInsertEquationCaptionCommand = class(TdxInsertCaptionCommandBase)
  protected
    class function GetInsertObjectCommandClass: TdxRichEditCommandClass; override;
    function GetPrefix: string; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxInsertFigureCaptionCommand }

  TdxInsertFigureCaptionCommand = class(TdxInsertCaptionCommandBase)
  protected
    class function GetInsertObjectCommandClass: TdxRichEditCommandClass; override;
    function GetPrefix: string; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxInsertTableCaptionCommand }

  TdxInsertTableCaptionCommand = class(TdxInsertCaptionCommandBase)
  protected
    class function GetInsertObjectCommandClass: TdxRichEditCommandClass; override;
    function GetPrefix: string; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxInsertCaptionCoreBaseCommand }

  TdxInsertCaptionCoreBaseCommand = class abstract(TdxInsertFieldCoreCommand);

  { TdxInsertEquationCaptionCoreCommand }

  TdxInsertEquationCaptionCoreCommand = class(TdxInsertCaptionCoreBaseCommand)
  public
    constructor Create(const AControl: IdxRichEditControl); reintroduce; override;

    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertFigureCaptionCoreCommand }

  TdxInsertFigureCaptionCoreCommand = class(TdxInsertCaptionCoreBaseCommand)
  public
    constructor Create(const AControl: IdxRichEditControl); reintroduce; override;

    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertTableCaptionCoreCommand }

  TdxInsertTableCaptionCoreCommand = class(TdxInsertCaptionCoreBaseCommand)
  public
    constructor Create(const AControl: IdxRichEditControl); reintroduce; override;

    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertCaptionPlaceholderCommand }

  TdxInsertCaptionPlaceholderCommand = class(TdxRichEditMenuItemSimpleCommand)
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

implementation

uses
  Classes, Contnrs, Math,
  dxCore,
  dxRichEdit.Commands.Strs,
  dxRichEdit.Commands.Images,
  dxRichEdit.DocumentModel.Fields.TocField,
  dxRichEdit.DocumentModel.FieldController;

{ TdxInsertTableOfContentsCoreBaseCommand }

procedure TdxInsertTableOfContentsCoreBaseCommand.UpdateField(AField: TdxField);
begin
  DocumentModel.ResetTemporaryLayout;
  inherited UpdateField(AField);
  DocumentModel.ResetTemporaryLayout;
  inherited UpdateField(AField);
  DocumentModel.ResetTemporaryLayout;
end;

procedure TdxInsertTableOfContentsCoreBaseCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  if AState.Enabled then
    AState.Enabled := ActivePieceTable.IsMain;
end;

{ TdxInsertTableOfContentsCoreCommand }

constructor TdxInsertTableOfContentsCoreCommand.Create(const AControl: IdxRichEditControl);
begin
  inherited Create(AControl, 'TOC \h');
end;

class function TdxInsertTableOfContentsCoreCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.InsertTableOfContents;
end;

class function TdxInsertTableOfContentsCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTableOfContentsDescription);
end;

class function TdxInsertTableOfContentsCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTableOfContentsMenuCaption);
end;

{ TdxInsertTableOfEquationsCoreCommand }

constructor TdxInsertTableOfEquationsCoreCommand.Create(const AControl: IdxRichEditControl);
begin
  inherited Create(AControl, 'TOC \h \c "Equation"');
end;

class function TdxInsertTableOfEquationsCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTableOfEquationsMenuCaption);
end;

class function TdxInsertTableOfEquationsCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTableOfEquationsDescription);
end;

class function TdxInsertTableOfEquationsCoreCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.InsertTableOfEquations;
end;

{ TdxInsertTableOfFiguresCoreCommand }

constructor TdxInsertTableOfFiguresCoreCommand.Create(const AControl: IdxRichEditControl);
begin
  inherited Create(AControl, 'TOC \h \c "Figure"');
end;

class function TdxInsertTableOfFiguresCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTableOfFiguresMenuCaption);
end;

class function TdxInsertTableOfFiguresCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTableOfFiguresDescription);
end;

class function TdxInsertTableOfFiguresCoreCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.InsertTableOfFigures;
end;

{ TdxInsertTableOfTablesCoreCommand }

constructor TdxInsertTableOfTablesCoreCommand.Create(const AControl: IdxRichEditControl);
begin
  inherited Create(AControl, 'TOC \h \c "Table"');
end;

class function TdxInsertTableOfTablesCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTableOfTablesMenuCaption);
end;

class function TdxInsertTableOfTablesCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTableOfTablesDescription);
end;

class function TdxInsertTableOfTablesCoreCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.InsertTableCaption;
end;

{ TdxInsertTableOfFiguresPlaceholderCommand }

class function TdxInsertTableOfFiguresPlaceholderCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTableOfFiguresPlaceholderMenuCaption);
end;

class function TdxInsertTableOfFiguresPlaceholderCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTableOfFiguresPlaceholderDescription);
end;

class function TdxInsertTableOfFiguresPlaceholderCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertTableOfFiguresPlaceholder;
end;

class function TdxInsertTableOfFiguresPlaceholderCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.InsertTableOfCaptions;
end;

procedure TdxInsertTableOfFiguresPlaceholderCommand.ExecuteCore;
begin
end;

procedure TdxInsertTableOfFiguresPlaceholderCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  AState.Enabled := ActivePieceTable.IsMain;
end;

{ TdxInsertTableOfContentsCommand }

class function TdxInsertTableOfContentsCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertTableOfContents;
end;

class function TdxInsertTableOfContentsCommand.GetInsertObjectCommandClass: TdxRichEditCommandClass;
begin
  Result := TdxInsertTableOfContentsCoreCommand;
end;

{ TdxInsertTableOfEquationsCommand }

class function TdxInsertTableOfEquationsCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertTableOfEquations;
end;

class function TdxInsertTableOfEquationsCommand.GetInsertObjectCommandClass: TdxRichEditCommandClass;
begin
  Result := TdxInsertTableOfEquationsCoreCommand
end;

{ TdxInsertTableOfFiguresCommand }

class function TdxInsertTableOfFiguresCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertTableOfFigures;
end;

class function TdxInsertTableOfFiguresCommand.GetInsertObjectCommandClass: TdxRichEditCommandClass;
begin
  Result := TdxInsertTableOfFiguresCoreCommand;
end;

{ TdxInsertTableOfTablesCommand }

class function TdxInsertTableOfTablesCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertTableOfTables;
end;

class function TdxInsertTableOfTablesCommand.GetInsertObjectCommandClass: TdxRichEditCommandClass;
begin
  Result := TdxInsertTableOfTablesCoreCommand;
end;

{ TdxChangeHeadingLevelCommandBase }

procedure TdxChangeHeadingLevelCommandBase.ChangeParagraphProperty(AParagraph: TdxParagraph);
var
  ALevel: Integer;
begin
  ALevel := CalculateHeadingLevel(AParagraph);
  if ALevel >= 0 then
    inherited ChangeParagraphProperty(AParagraph)
  else
  begin
    ALevel := CalculateOutlineLevel(AParagraph);
    if (ALevel >= 0) and (ALevel <= 9) then
      AParagraph.ParagraphProperties.OutlineLevel := ALevel;
  end;
end;

function TdxChangeHeadingLevelCommandBase.CalculateParagraphStyleIndex(AParagraph: TdxParagraph): Integer;
var
  ALevel: Integer;
begin
  ALevel := CalculateHeadingLevel(AParagraph);
  if ALevel < 0 then
    Exit(AParagraph.ParagraphStyleIndex);

  if ALevel = 0 then
    Exit(DocumentModel.ParagraphStyles.DefaultItemIndex);

  Result := DocumentModel.ParagraphStyles.GetHeadingParagraphStyle(ALevel);
end;

{ TdxIncrementParagraphOutlineLevelCommand }

class function TdxIncrementParagraphOutlineLevelCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandIncrementParagraphOutlineLevelMenuCaption);
end;

class function TdxIncrementParagraphOutlineLevelCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandIncrementParagraphOutlineLevelDescription);
end;

class function TdxIncrementParagraphOutlineLevelCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.IncrementParagraphOutlineLevel;
end;

function TdxIncrementParagraphOutlineLevelCommand.CalculateHeadingLevel(AParagraph: TdxParagraph): Integer;
var
  ALevel: Integer;
begin
  ALevel := AParagraph.ParagraphProperties.OutlineLevel;
  if ALevel <> 0 then
    Exit(-1);

  ALevel := AParagraph.ParagraphStyle.ParagraphProperties.OutlineLevel;
  if ALevel = 0 then
    Exit(1);
  Result := Min(9, Math.Max(ALevel + 1, 1));
end;

function TdxIncrementParagraphOutlineLevelCommand.CalculateOutlineLevel(AParagraph: TdxParagraph): Integer;
var
  ALevel: Integer;
begin
  ALevel := AParagraph.ParagraphProperties.OutlineLevel;
  Result := Math.Min(9, Math.Max(ALevel + 1, 0));
end;

{ TdxDecrementParagraphOutlineLevelCommand }

class function TdxDecrementParagraphOutlineLevelCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDecrementParagraphOutlineLevelMenuCaption);
end;

class function TdxDecrementParagraphOutlineLevelCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandDecrementParagraphOutlineLevelDescription);
end;

class function TdxDecrementParagraphOutlineLevelCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.DecrementParagraphOutlineLevel;
end;

function TdxDecrementParagraphOutlineLevelCommand.CalculateHeadingLevel(AParagraph: TdxParagraph): Integer;
var
  ALevel: Integer;
begin
  ALevel := AParagraph.ParagraphProperties.OutlineLevel;
  if ALevel <> 0 then
    Exit(-1);

  ALevel := AParagraph.ParagraphStyle.ParagraphProperties.OutlineLevel;
  if ALevel = 0 then
    Exit(1);
  Result := Math.Min(9, Math.Max(ALevel - 1, 1));
end;

function TdxDecrementParagraphOutlineLevelCommand.CalculateOutlineLevel(AParagraph: TdxParagraph): Integer;
var
  ALevel: Integer;
begin
  ALevel := AParagraph.ParagraphProperties.OutlineLevel;
  Result := Math.Min(9, Math.Max(ALevel - 1, 0));
end;

{ TdxSetParagraphHeadingLevelCommandBase }

class function TdxSetParagraphHeadingLevelCommandBase.GetDescription: string;
begin
  Result := Format(cxGetResourceString(@sdxRichEditCommandSetParagraphHeadingLevelDescription), [GetLevel]);
end;

class function TdxSetParagraphHeadingLevelCommandBase.GetMenuCaption: string;
begin
  Result := Format(cxGetResourceString(@sdxRichEditCommandSetParagraphHeadingLevelMenuCaption), [GetLevel]);
end;

function TdxSetParagraphHeadingLevelCommandBase.CalculateHeadingLevel(AParagraph: TdxParagraph): Integer;
begin
  Result := GetLevel;
end;

function TdxSetParagraphHeadingLevelCommandBase.CalculateOutlineLevel(AParagraph: TdxParagraph): Integer;
begin
  Result := GetLevel;
end;

{ TdxSetParagraphHeading1LevelCommand }

class function TdxSetParagraphHeading1LevelCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetParagraphHeading1Level;
end;

class function TdxSetParagraphHeading1LevelCommand.GetLevel: Integer;
begin
  Result := 1;
end;

{ TdxSetParagraphHeading2LevelCommand }

class function TdxSetParagraphHeading2LevelCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetParagraphHeading2Level;
end;

class function TdxSetParagraphHeading2LevelCommand.GetLevel: Integer;
begin
  Result := 2;
end;

{ TdxSetParagraphHeading3LevelCommand }

class function TdxSetParagraphHeading3LevelCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetParagraphHeading3Level;
end;

class function TdxSetParagraphHeading3LevelCommand.GetLevel: Integer;
begin
  Result := 3;
end;

{ TdxSetParagraphHeading4LevelCommand }

class function TdxSetParagraphHeading4LevelCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetParagraphHeading4Level;
end;

class function TdxSetParagraphHeading4LevelCommand.GetLevel: Integer;
begin
  Result := 4;
end;

{ TdxSetParagraphHeading5LevelCommand }

class function TdxSetParagraphHeading5LevelCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetParagraphHeading5Level;
end;

class function TdxSetParagraphHeading5LevelCommand.GetLevel: Integer;
begin
  Result := 5;
end;

{ TdxSetParagraphHeading6LevelCommand }

class function TdxSetParagraphHeading6LevelCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetParagraphHeading6Level;
end;

class function TdxSetParagraphHeading6LevelCommand.GetLevel: Integer;
begin
  Result := 6;
end;

{ TdxSetParagraphHeading7LevelCommand }

class function TdxSetParagraphHeading7LevelCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetParagraphHeading7Level;
end;

class function TdxSetParagraphHeading7LevelCommand.GetLevel: Integer;
begin
  Result := 7;
end;

{ TdxSetParagraphHeading8LevelCommand }

class function TdxSetParagraphHeading8LevelCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetParagraphHeading8Level;
end;

class function TdxSetParagraphHeading8LevelCommand.GetLevel: Integer;
begin
  Result := 8;
end;

{ TdxSetParagraphHeading9LevelCommand }

class function TdxSetParagraphHeading9LevelCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetParagraphHeading9Level;
end;

class function TdxSetParagraphHeading9LevelCommand.GetLevel: Integer;
begin
  Result := 9;
end;

{ TdxSetParagraphBodyTextLevelCommand }

class function TdxSetParagraphBodyTextLevelCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetParagraphBodyTextLevelMenuCaption);
end;

class function TdxSetParagraphBodyTextLevelCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetParagraphBodyTextLevelDescription);
end;

class function TdxSetParagraphBodyTextLevelCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetParagraphBodyTextLevel;
end;

class function TdxSetParagraphBodyTextLevelCommand.GetLevel: Integer;
begin
  Result := 0;
end;

{ TdxAddParagraphsToTableOfContentsCommand }

class function TdxAddParagraphsToTableOfContentsCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandAddParagraphsToTableOfContentsMenuCaption);
end;

class function TdxAddParagraphsToTableOfContentsCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandAddParagraphsToTableOfContentsDescription);
end;

class function TdxAddParagraphsToTableOfContentsCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.AddParagraphsToTableOfContents;
end;

class function TdxAddParagraphsToTableOfContentsCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.AddParagraphToTableOfContents;
end;

procedure TdxAddParagraphsToTableOfContentsCommand.ExecuteCore;
begin
end;

procedure TdxAddParagraphsToTableOfContentsCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  if not ActivePieceTable.CanContainCompositeContent then
    AState.Enabled := False
  else
  begin
    AState.Enabled := IsContentEditable;
    AState.Visible := True;
    ApplyCommandRestrictionOnEditableControl(AState, DocumentModel.DocumentCapabilities.ParagraphStyle, AState.Enabled);
    ApplyDocumentProtectionToSelectedParagraphs(AState);
  end;
end;

{ TdxUpdateTableOfContentsCommand }

constructor TdxUpdateTableOfContentsCommand.Create(const AControl: IdxRichEditControl);
begin
  inherited Create(AControl, nil);
end;

class function TdxUpdateTableOfContentsCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.UpdateTableOfContents;
end;

class function TdxUpdateTableOfContentsCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandUpdateTableOfContentsMenuCaption);
end;

class function TdxUpdateTableOfContentsCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandUpdateTableOfContentsDescription);
end;

class function TdxUpdateTableOfContentsCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.UpdateTableOfContents;
end;

procedure TdxUpdateTableOfContentsCommand.ExecuteCore;
var
  APieceTable: TdxPieceTable;
  AFields: TdxFieldList;
  ACount, I: Integer;
  AUpdater: TdxFieldUpdater;
begin
  DocumentModel.ResetTemporaryLayout;

  APieceTable := DocumentModel.MainPieceTable;
  AFields := APieceTable.GetTocFields;
  try
    ACount := AFields.Count;
    AUpdater := APieceTable.FieldUpdater;
    for I := 0 to ACount - 1 do
    begin
      AUpdater.UpdateFieldAndNestedFields(AFields[I]);
      DocumentModel.ResetTemporaryLayout;
    end;
  finally
    AFields.Free;
  end;
end;

procedure TdxUpdateTableOfContentsCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  AState.Enabled := IsContentEditable;
  AState.Visible := True;
  if not ActivePieceTable.CanContainCompositeContent then
    AState.Enabled := False;
end;

{ TdxUpdateTableOfFiguresCommand }

class function TdxUpdateTableOfFiguresCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.UpdateTableOfFigures;
end;

class function TdxUpdateTableOfFiguresCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandUpdateTableOfFiguresMenuCaption);
end;

class function TdxUpdateTableOfFiguresCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandUpdateTableOfFiguresDescription);
end;

procedure TdxUpdateTableOfFiguresCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  if not ActivePieceTable.CanContainCompositeContent then
    AState.Enabled := False;
end;

{ TdxInsertCaptionCommandBase }

function TdxInsertCaptionCommandBase.GetInsertObjectCommand: TdxRichEditCommand;
begin
  Result := TdxRichEditCommand(Commands[2]);
end;

procedure TdxInsertCaptionCommandBase.CreateCommands;
var
  ACommand: TdxInsertTextCommand;
begin
  inherited CreateCommands;
  ACommand := TdxInsertTextCommand(InnerControl.CreateCommand(TdxRichEditCommandId.InsertText));
  ACommand.Text := Prefix + ' ';
  Commands.Insert(1, TdxRichEditCommand(ACommand));
end;

{ TdxInsertEquationCaptionCommand }

class function TdxInsertEquationCaptionCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertEquationCaption;
end;

function TdxInsertEquationCaptionCommand.GetPrefix: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandCaptionPrefixEquation);
end;

class function TdxInsertEquationCaptionCommand.GetInsertObjectCommandClass: TdxRichEditCommandClass;
begin
  Result := TdxInsertEquationCaptionCoreCommand;
end;

{ TdxInsertFigureCaptionCommand }

class function TdxInsertFigureCaptionCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertFigureCaption;
end;

function TdxInsertFigureCaptionCommand.GetPrefix: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandCaptionPrefixFigure);
end;

class function TdxInsertFigureCaptionCommand.GetInsertObjectCommandClass: TdxRichEditCommandClass;
begin
  Result := TdxInsertFigureCaptionCoreCommand;
end;

{ TdxInsertTableCaptionCommand }

class function TdxInsertTableCaptionCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertTableCaption;
end;

function TdxInsertTableCaptionCommand.GetPrefix: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandCaptionPrefixTable);
end;

class function TdxInsertTableCaptionCommand.GetInsertObjectCommandClass: TdxRichEditCommandClass;
begin
  Result := TdxInsertTableCaptionCoreCommand;
end;

{ TdxInsertEquationCaptionCoreCommand }

constructor TdxInsertEquationCaptionCoreCommand.Create(const AControl: IdxRichEditControl);
begin
  inherited Create(AControl, 'SEQ Equation \* ARABIC');
end;

class function TdxInsertEquationCaptionCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertEquationCaptionMenuCaption);
end;

class function TdxInsertEquationCaptionCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertEquationCaptionDescription);
end;

class function TdxInsertEquationCaptionCoreCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.InsertEquationCaption;
end;

{ TdxInsertFigureCaptionCoreCommand }

constructor TdxInsertFigureCaptionCoreCommand.Create(const AControl: IdxRichEditControl);
begin
  inherited Create(AControl, 'SEQ Figure \* ARABIC');
end;

class function TdxInsertFigureCaptionCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertFigureCaptionMenuCaption);
end;

class function TdxInsertFigureCaptionCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertFigureCaptionDescription);
end;

class function TdxInsertFigureCaptionCoreCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.InsertFigureCaption;
end;

{ TdxInsertTableCaptionCoreCommand }

constructor TdxInsertTableCaptionCoreCommand.Create(const AControl: IdxRichEditControl);
begin
  inherited Create(AControl, 'SEQ Table \* ARABIC');
end;

class function TdxInsertTableCaptionCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTableCaptionMenuCaption);
end;

class function TdxInsertTableCaptionCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTableCaptionDescription);
end;

class function TdxInsertTableCaptionCoreCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.InsertTableCaption;
end;

{ TdxInsertCaptionPlaceholderCommand }

class function TdxInsertCaptionPlaceholderCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertCaptionPlaceholderMenuCaption);
end;

class function TdxInsertCaptionPlaceholderCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertCaptionPlaceholderDescription);
end;

class function TdxInsertCaptionPlaceholderCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertCaptionPlaceholder;
end;

class function TdxInsertCaptionPlaceholderCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.InsertCaption;
end;

procedure TdxInsertCaptionPlaceholderCommand.ExecuteCore;
begin
end;

procedure TdxInsertCaptionPlaceholderCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  if AState.Enabled then
    AState.Enabled := ActivePieceTable.IsMain;
end;

end.
