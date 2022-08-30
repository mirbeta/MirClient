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

unit dxRichEdit.Commands.Keyboard;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Graphics, Windows, Classes, Controls, Generics.Defaults, Generics.Collections,
  dxCoreClasses,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.Utils.DataObject,
  dxRichEdit.View.ViewInfo,
  dxRichEdit.View.Core,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Commands,
  dxRichEdit.DocumentModel.PieceTableIterators,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.Control.HitTest,
  dxRichEdit.Commands,
  dxRichEdit.Commands.Insert,
  dxRichEdit.Commands.IDs,
  dxRichEdit.Options,
  dxRichEdit.Commands.MultiCommand;

type
  { TdxEnterKeyCommand }

  TdxEnterKeyCommand = class(TdxMultiCommand)
  protected
    procedure CreateCommands; override;
    function GetExecutionMode: TdxMultiCommandExecutionMode; override;
    function GetUpdateUIStateMode: TdxMultiCommandUpdateUIStateMode; override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxOpenHyperlinkAtCaretPositionCommand }

  TdxOpenHyperlinkAtCaretPositionCommand = class(TdxRichEditMenuItemSimpleCommand)
  strict private
    FHyperlinkField: TdxField;
  private
    function GetHyperlinkField: TdxField;
  protected
    function GetHyperlinkFieldAtCaretPosition: TdxField;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
    property HyperlinkField: TdxField read GetHyperlinkField;
  public
    procedure ExecuteCore; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertCopyrightSymbolCommand }

  TdxInsertCopyrightSymbolCommand = class(TdxTransactedInsertObjectCommand)
  protected
    class function GetInsertObjectCommandClass: TdxRichEditCommandClass; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxInsertCopyrightSymbolCoreCommand }

  TdxInsertCopyrightSymbolCoreCommand = class(TdxInsertSpecialCharacterCommandBase)
  protected
    function GetCharacter: Char; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertRegisteredTrademarkSymbolCommand }

  TdxInsertRegisteredTrademarkSymbolCommand = class(TdxTransactedInsertObjectCommand)
  protected
    class function GetInsertObjectCommandClass: TdxRichEditCommandClass; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxInsertRegisteredTrademarkSymbolCoreCommand }

  TdxInsertRegisteredTrademarkSymbolCoreCommand = class(TdxInsertSpecialCharacterCommandBase)
  protected
    function GetCharacter: Char; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertTrademarkSymbolCommand }

  TdxInsertTrademarkSymbolCommand = class(TdxTransactedInsertObjectCommand)
  protected
    class function GetInsertObjectCommandClass: TdxRichEditCommandClass; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxInsertTrademarkSymbolCoreCommand }

  TdxInsertTrademarkSymbolCoreCommand = class(TdxInsertSpecialCharacterCommandBase)
  protected
    function GetCharacter: Char; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertEllipsisCommand }

  TdxInsertEllipsisCommand = class(TdxTransactedInsertObjectCommand)
  protected
    class function GetInsertObjectCommandClass: TdxRichEditCommandClass; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

  { TdxInsertEllipsisCoreCommand }

  TdxInsertEllipsisCoreCommand = class(TdxInsertSpecialCharacterCommandBase)
  protected
    function GetCharacter: Char; override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxToggleOvertypeCommand }

  TdxToggleOvertypeCommand = class(TdxRichEditMenuItemSimpleCommand)
  protected
    procedure ExecuteCore; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function Id: TdxRichEditCommandId; override;
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertTabCoreCommand }

  TdxInsertTabCoreCommand = class(TdxInsertSpecialCharacterCommandBase)
  protected
    function GetCharacter: Char; override;
    function GetInsertedText: string; override;
    procedure UpdateUIStateCore(const AState: IdxCommandUIState); override;
  public
    class function GetDescription: string; override;
    class function GetMenuCaption: string; override;
  end;

  { TdxInsertTabCommand }

  TdxInsertTabCommand = class(TdxTransactedInsertObjectCommand)
  protected
    class function GetInsertObjectCommandClass: TdxRichEditCommandClass; override;
  public
    class function Id: TdxRichEditCommandId; override;
  end;

implementation

uses
  Contnrs, Math, dxCore, cxGeometry, dxTypeHelpers,
  dxRichEdit.DocumentModel.TabFormatting,
  dxRichEdit.DocumentModel.ParagraphRange,
  dxRichEdit.DocumentModel.Styles,
  dxRichEdit.View.PageViewInfoGenerator,
  dxRichEdit.DocumentLayout.Position,
  dxRichEdit.DocumentLayout.UnitConverter,
  dxCharacters,
  dxRichEdit.Utils.CheckSumStream,
  dxRichEdit.Commands.Strs,
  dxRichEdit.Commands.Selection,
  dxRichEdit.Commands.Delete,
  dxRichEdit.Commands.CopyAndPaste;

{ TdxEnterKeyCommand }

procedure TdxEnterKeyCommand.CreateCommands;
begin
  Commands.Add(TdxOpenHyperlinkAtCaretPositionCommand.Create(RichEditControl));
  Commands.Add(TdxInsertParagraphCommand.Create(RichEditControl));
end;

function TdxEnterKeyCommand.GetExecutionMode: TdxMultiCommandExecutionMode;
begin
  Result := TdxMultiCommandExecutionMode.ExecuteFirstAvailable;
end;

class function TdxEnterKeyCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandEnterKeyDescription);
end;

class function TdxEnterKeyCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandEnterKeyMenuCaption);
end;

class function TdxEnterKeyCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.EnterKey;
end;

function TdxEnterKeyCommand.GetUpdateUIStateMode: TdxMultiCommandUpdateUIStateMode;
begin
  Result := TdxMultiCommandUpdateUIStateMode.EnableIfAnyAvailable;
end;

{ TdxOpenHyperlinkAtCaretPositionCommand }

procedure TdxOpenHyperlinkAtCaretPositionCommand.ExecuteCore;
begin
  if HyperlinkField = nil then
    Exit;
  InnerControl.OnHyperlinkClick(HyperlinkField, False);
end;

class function TdxOpenHyperlinkAtCaretPositionCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandOpenHyperlinkAtCaretPositionDescription);
end;

function TdxOpenHyperlinkAtCaretPositionCommand.GetHyperlinkField: TdxField;
begin
  if FHyperlinkField = nil then
    FHyperlinkField := GetHyperlinkFieldAtCaretPosition;
  Result := FHyperlinkField;
end;

function TdxOpenHyperlinkAtCaretPositionCommand.GetHyperlinkFieldAtCaretPosition: TdxField;
var
  ALayoutPosition: TdxDocumentLayoutPosition;
  ARunIndex: TdxRunIndex;
  AField: TdxField;
  APosition: TdxDocumentModelPosition;
begin
  Result := nil;
  ALayoutPosition := ActiveView.CaretPosition.LayoutPosition;
  if ALayoutPosition.DetailsLevel = TdxDocumentLayoutDetailsLevel.Character then
  begin
    ARunIndex := ALayoutPosition.Character.StartPos.RunIndex;
    AField := ActivePieceTable.GetHyperlinkField(ARunIndex);
    if (AField <> nil) and not AField.IsCodeView then
    begin
      APosition := TdxDocumentModelPosition.Create(ActivePieceTable);
      APosition.LogPosition := ALayoutPosition.LogPosition;
      APosition.Update;
      if (APosition.RunIndex = AField.Code.Start) and (APosition.RunOffset = 0) then
        Exit;
      Result := AField;
    end;
  end;
end;

class function TdxOpenHyperlinkAtCaretPositionCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandOpenHyperlinkAtCaretPositionMenuCaption);
end;

procedure TdxOpenHyperlinkAtCaretPositionCommand.UpdateUIStateCore(const AState: IdxCommandUIState);
begin
  CheckExecutedAtUIThread;
  AState.Enabled := HyperlinkField <> nil;
  AState.Visible := True;
end;

{ TdxInsertCopyrightSymbolCommand }

class function TdxInsertCopyrightSymbolCommand.GetInsertObjectCommandClass: TdxRichEditCommandClass;
begin
  Result := TdxInsertCopyrightSymbolCoreCommand;
end;

class function TdxInsertCopyrightSymbolCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertCopyrightSymbol;
end;

{ TdxInsertCopyrightSymbolCoreCommand }

function TdxInsertCopyrightSymbolCoreCommand.GetCharacter: Char;
begin
  Result := TdxCharacters.CopyrightSymbol;
end;

class function TdxInsertCopyrightSymbolCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertCopyrightSymbolDescription);
end;

class function TdxInsertCopyrightSymbolCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertCopyrightSymbolMenuCaption);
end;

{ TdxInsertRegisteredTrademarkSymbolCommand }

class function TdxInsertRegisteredTrademarkSymbolCommand.GetInsertObjectCommandClass: TdxRichEditCommandClass;
begin
  Result := TdxInsertRegisteredTrademarkSymbolCoreCommand;
end;

class function TdxInsertRegisteredTrademarkSymbolCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertRegisteredTrademarkSymbol;
end;

{ TdxInsertRegisteredTrademarkSymbolCoreCommand }

function TdxInsertRegisteredTrademarkSymbolCoreCommand.GetCharacter: Char;
begin
  Result := TdxCharacters.RegisteredTrademarkSymbol;
end;

class function TdxInsertRegisteredTrademarkSymbolCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertRegisteredTrademarkSymbolDescription);
end;

class function TdxInsertRegisteredTrademarkSymbolCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertRegisteredTrademarkSymbolMenuCaption);
end;

{ TdxInsertTrademarkSymbolCommand }

class function TdxInsertTrademarkSymbolCommand.GetInsertObjectCommandClass: TdxRichEditCommandClass;
begin
  Result := TdxInsertTrademarkSymbolCoreCommand;
end;

class function TdxInsertTrademarkSymbolCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertTrademarkSymbol;
end;

{ TdxInsertTrademarkSymbolCoreCommand }

function TdxInsertTrademarkSymbolCoreCommand.GetCharacter: Char;
begin
  Result := TdxCharacters.TrademarkSymbol;
end;

class function TdxInsertTrademarkSymbolCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTrademarkSymbolDescription);
end;

class function TdxInsertTrademarkSymbolCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTrademarkSymbolMenuCaption);
end;

{ TdxInsertEllipsisCommand }

class function TdxInsertEllipsisCommand.GetInsertObjectCommandClass: TdxRichEditCommandClass;
begin
  Result := TdxInsertEllipsisCoreCommand;
end;

class function TdxInsertEllipsisCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.InsertEllipsis;
end;

{ TdxInsertEllipsisCoreCommand }

function TdxInsertEllipsisCoreCommand.GetCharacter: Char;
begin
  Result := TdxCharacters.Ellipsis;
end;

class function TdxInsertEllipsisCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertEllipsisDescription);
end;

class function TdxInsertEllipsisCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertEllipsisMenuCaption);
end;

{ TdxToggleOvertypeCommand }

procedure TdxToggleOvertypeCommand.ExecuteCore;
begin
  RichEditControl.Overtype := not RichEditControl.Overtype;
end;

class function TdxToggleOvertypeCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleOvertypeDescription);
end;

class function TdxToggleOvertypeCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandToggleOvertypeMenuCaption);
end;

class function TdxToggleOvertypeCommand.Id: TdxRichEditCommandId;
begin
  Result := TdxRichEditCommandId.ToggleOvertype;
end;

procedure TdxToggleOvertypeCommand.UpdateUIStateCore(
  const AState: IdxCommandUIState);
begin
  AState.Enabled := Options.Behavior.OvertypeAllowed;
  AState.Visible := True;
  AState.Checked := RichEditControl.Overtype;
end;

{ TdxInsertTabCoreCommand }

function TdxInsertTabCoreCommand.GetCharacter: Char;
begin
  Result := TdxCharacters.TabMark;
end;

class function TdxInsertTabCoreCommand.GetDescription: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTabDescription);
end;

function TdxInsertTabCoreCommand.GetInsertedText: string;
begin
  Result := InnerControl.Options.Behavior.TabMarker;
end;

class function TdxInsertTabCoreCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxRichEditCommandInsertTabMenuCaption);
end;

procedure TdxInsertTabCoreCommand.UpdateUIStateCore(
  const AState: IdxCommandUIState);
begin
  inherited UpdateUIStateCore(AState);
  ApplyCommandRestrictionOnEditableControl(AState, DocumentModel.DocumentCapabilities.TabSymbol, AState.Enabled);
  ApplyDocumentProtectionToSelectedCharacters(AState);
end;

{ TdxInsertTabCommand }

class function TdxInsertTabCommand.GetInsertObjectCommandClass: TdxRichEditCommandClass;
begin
  Result := TdxInsertTabCoreCommand;
end;

class function TdxInsertTabCommand.Id: TdxRichEditCommandId;
begin
    Result := TdxRichEditCommandId.InsertTab;
end;

end.
