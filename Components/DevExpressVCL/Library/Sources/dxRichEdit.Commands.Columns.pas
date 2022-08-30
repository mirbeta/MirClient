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

unit dxRichEdit.Commands.Columns;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  dxRichEdit.View.Core,
  dxRichEdit.Commands.IDs,
  dxRichEdit.Commands.Selection,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple;

type
  { TdxSetSectionColumnCountCommandBase }

  TdxSetSectionColumnCountCommandBase = class abstract(TdxSelectionBasedPropertyChangeCommandBase)
  protected
    function ChangeProperty(const AStart: TdxDocumentModelPosition;
      const AEnd: TdxDocumentModelPosition;
      const AState: IdxCommandUIState): TdxDocumentModelChangeActions; override;
    procedure ModifyDocumentModelCore(const AState: IdxCommandUIState); override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); overload; override;

    function GetActualColumnCount: Integer; virtual; abstract;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState; ASection: TdxSection); reintroduce; overload; virtual;

    property ActualColumnCount: Integer read GetActualColumnCount;
  end;

  { TdxSetSectionOneColumnCommand }

  TdxSetSectionOneColumnCommand = class(TdxSetSectionColumnCountCommandBase)
  protected
    function GetActualColumnCount: Integer; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxSetSectionTwoColumnsCommand }

  TdxSetSectionTwoColumnsCommand = class(TdxSetSectionColumnCountCommandBase)
  protected
    function GetActualColumnCount: Integer; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxSetSectionThreeColumnsCommand }

  TdxSetSectionThreeColumnsCommand = class(TdxSetSectionColumnCountCommandBase)
  protected
    function GetActualColumnCount: Integer; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetImageName: string; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxSetSectionColumnsPlaceholderCommand }

  TdxSetSectionColumnsPlaceholderCommand = class(TdxSetSectionColumnCountCommandBase)
  protected
    function GetActualColumnCount: Integer; override;
    procedure ModifyDocumentModelCore(const AState: IdxCommandUIState); override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState; ASection: TdxSection); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

implementation

uses
  dxCore,
  dxRichEdit.DocumentModel.SectionFormatting,
  dxRichEdit.Commands.Images,
  dxRichEdit.Commands.Strs,
  dxRichEdit.Utils.Types;

{ TdxSetSectionColumnCountCommandBase }

function TdxSetSectionColumnCountCommandBase.ChangeProperty(const AStart,
  AEnd: TdxDocumentModelPosition;
  const AState: IdxCommandUIState): TdxDocumentModelChangeActions;
begin
  Result := [];
end;

procedure TdxSetSectionColumnCountCommandBase.ModifyDocumentModelCore(
  const AState: IdxCommandUIState);
var
  ADocumentModel: TdxDocumentModel;
  ASection: TdxSection;
  AColumns: TdxSectionColumns;
  AStartParagraphIndex: TdxParagraphIndex;
  AEndParagraphIndex: TdxParagraphIndex;
  APieceTable: TdxPieceTable;
  AParagraphs: TdxParagraphCollection;
begin
  ADocumentModel := DocumentModel;
  ASection := ADocumentModel.GetActiveSectionBySelectionEnd;
  if ASection = nil then
    Exit;
  AColumns := ASection.Columns;
  if ActualColumnCount <> AColumns.ColumnCount then
  begin
    AColumns.ColumnCount := ActualColumnCount;
    AColumns.EqualWidthColumns := True;
    AStartParagraphIndex := ASection.FirstParagraphIndex;
    AEndParagraphIndex := ASection.LastParagraphIndex;
    APieceTable := ADocumentModel.MainPieceTable;
    AParagraphs := APieceTable.Paragraphs;
    APieceTable.ApplyChanges(TdxDocumentModelChangeType.ModifySection,
      AParagraphs[AStartParagraphIndex].FirstRunIndex, AParagraphs[AEndParagraphIndex].LastRunIndex);
  end;
end;

procedure TdxSetSectionColumnCountCommandBase.UpdateUIStateCore(
  const AState: IdxCommandUIState);
var
  ASection: TdxSection;
  AColumns: TdxSectionColumns;
begin
  ASection := DocumentModel.GetActiveSectionBySelectionEnd;

  AColumns := ASection.Columns;
  AState.Enabled := IsContentEditable and (ASection <> nil) and DocumentModel.CanEditSection(ASection);
  AState.Checked := AColumns.EqualWidthColumns and (AColumns.ColumnCount = ActualColumnCount);
  AState.Visible := True;

  ApplyCommandRestrictionOnEditableControl(AState, DocumentModel.DocumentCapabilities.Sections, AState.Enabled);
  if ASection <> nil then
    UpdateUIStateCore(AState, ASection);
end;

procedure TdxSetSectionColumnCountCommandBase.UpdateUIStateCore(
  const AState: IdxCommandUIState; ASection: TdxSection);
begin
end;

{ TdxSetSectionOneColumnCommand }

function TdxSetSectionOneColumnCommand.GetActualColumnCount: Integer;
begin
  Result := 1;
end;

class function TdxSetSectionOneColumnCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetSectionOneColumnDescription)
end;

class function TdxSetSectionOneColumnCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetSectionOneColumnMenuCaption)
end;

class function TdxSetSectionOneColumnCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetSectionOneColumn;
end;

class function TdxSetSectionOneColumnCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.SetSectionOneColumn;
end;

{ TdxSetSectionTwoColumnsCommand }

function TdxSetSectionTwoColumnsCommand.GetActualColumnCount: Integer;
begin
  Result := 2;
end;

class function TdxSetSectionTwoColumnsCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetSectionTwoColumnsDescription)
end;

class function TdxSetSectionTwoColumnsCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetSectionTwoColumnsMenuCaption)
end;

class function TdxSetSectionTwoColumnsCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetSectionTwoColumns;
end;

class function TdxSetSectionTwoColumnsCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.SetSectionTwoColumns;
end;

{ TdxSetSectionThreeColumnsCommand }

function TdxSetSectionThreeColumnsCommand.GetActualColumnCount: Integer;
begin
  Result := 3;
end;

class function TdxSetSectionThreeColumnsCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetSectionThreeColumnsDescription)
end;

class function TdxSetSectionThreeColumnsCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetSectionThreeColumnsMenuCaption)
end;

class function TdxSetSectionThreeColumnsCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetSectionThreeColumns;
end;

class function TdxSetSectionThreeColumnsCommand.GetImageName: string;
begin
  Result := TdxRichEditControlCommandsImages.SetSectionThreeColumns;
end;

{ TdxSetSectionColumnsPlaceholderCommand }

function TdxSetSectionColumnsPlaceholderCommand.GetActualColumnCount: Integer;
begin
  Result := -1;
end;

class function TdxSetSectionColumnsPlaceholderCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetSectionColumnsDescription)
end;

class function TdxSetSectionColumnsPlaceholderCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandSetSectionColumnsDescription)
end;

class function TdxSetSectionColumnsPlaceholderCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.SetSectionColumnsPlaceholder;
end;

procedure TdxSetSectionColumnsPlaceholderCommand.ModifyDocumentModelCore(
  const AState: IdxCommandUIState);
begin
end;

procedure TdxSetSectionColumnsPlaceholderCommand.UpdateUIStateCore(const AState: IdxCommandUIState;
  ASection: TdxSection);
begin
  inherited UpdateUIStateCore(AState);
  if not RichEditControl.InnerControl.DocumentModel.ActivePieceTable.CanContainCompositeContent then
    AState.Enabled := False;
end;

end.
