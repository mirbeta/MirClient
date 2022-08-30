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

unit dxRichEdit.Control.MenuBuilder;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Types, Windows, Messages, Classes, SysUtils, Graphics, Generics.Defaults, Generics.Collections, Controls,
  dxCore, dxCoreClasses, cxGraphics, dxGDIPlusClasses, dxBuiltInPopupMenu,

  dxRichEdit.Commands.IDs,
  dxRichEdit.View.Core,
  dxRichEdit.Platform.Win.Control,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Fields.Core,
  dxRichEdit.DocumentModel.Fields,
  dxRichEdit.DocumentModel.PieceTable;

type
  { TdxRichEditPopupMenuImagesManager }

  TdxRichEditPopupMenuImagesManager = class
  private
    class var FImages: TcxImageList;
    class procedure Finalize; static;
    class function GetImages: TcxImageList; static;
  protected
    class procedure AddImagesFromResource(const AResourceName: string; AImages: TcxImageList); static;
    class function CreateImageList: TcxImageList; virtual;
    class function GetImageIndex(ACommandID: TdxRichEditCommandID): Integer; virtual;
    class procedure PopulateImages(AImages: TcxImageList); virtual;
  public
    class property Images: TcxImageList read GetImages;
  end;

  TdxRichEditPopupMenuImagesManagerClass = class of TdxRichEditPopupMenuImagesManager;

  { TdxRichEditCustomPopupMenu }

  TdxRichEditCustomPopupMenu = class abstract (TComponent)
  protected
    class function GetManagerClass: TdxRichEditPopupMenuImagesManagerClass; virtual;
  strict private
    FAdapter: TdxCustomBuiltInPopupMenuAdapter;
    FControl: TdxVCLControl;
    FCommands: TdxFastObjectList;
    function GetInnerControl: IdxInnerControl;
    function GetRichEditControl: IdxRichEditControl;
  protected
    function AddCommand(ACommandID: TdxRichEditCommandID; AIsSubMenu: Boolean; AParent: TComponent; AIsCheckItem: Boolean = False): TComponent; overload;
    function AddCommand(ACommand: TdxRichEditCommand; AIsSubMenu: Boolean; AParent: TComponent; AIsCheckItem: Boolean = False): TComponent; overload;
    procedure MenuItemClick(Sender: TObject);
    procedure PopulateMenuItems; virtual; abstract;
    procedure ProcessClick(ACommand: TdxRichEditCommand); virtual;

    property Control: TdxVCLControl read FControl;
    property InnerControl: IdxInnerControl read GetInnerControl;
    property RichEditControl: IdxRichEditControl read GetRichEditControl;
  public
    constructor Create(AControl: TdxVCLControl); reintroduce; virtual;
    destructor Destroy; override;

    function AddMenuItem(ACommandID: TdxRichEditCommandID; AParent: TComponent = nil): TComponent;
    function AddMenuCheckItem(ACommandID: TdxRichEditCommandID; AParent: TComponent = nil): TComponent;
    function AddSubMenu(ACommandID: TdxRichEditCommandID; AParent: TComponent = nil): TComponent;
    procedure Initialize;
    function Popup(const P: TPoint): Boolean;

    property Adapter: TdxCustomBuiltInPopupMenuAdapter read FAdapter;
  end;

  TdxRichEditCustomPopupMenuClass = class of TdxRichEditCustomPopupMenu;

  { TdxRichEditContentPopupMenu }

  TdxRichEditContentPopupMenu = class(TdxRichEditCustomPopupMenu)
  private
    function GetSelectedField: TdxField;
    function GetInvisibleField(const ASelectionStart: TdxDocumentModelPosition): TdxField;
  protected
    function AddFieldMenu: Boolean;
    procedure AddClipboardMenuItems;
    procedure AddFieldMenuItems;
    procedure AddFloatingObjectTextWrapTypeSubmenu;
    procedure AddFloatingObjectBringForwardSubmenu;
    procedure AddFloatingObjectSendBackwardSubmenu;
    procedure AddHyperlinkMenuItems;
    procedure AddSpellCheckerMenuItems;
    procedure AddTableInsertSubmenu;
    procedure AddTableCellsAlignmentSubmenu;
    procedure AddTableAutoFitSubmenu;
    procedure AddTableOptionsMenuItems;
    procedure PopulateMenuItems; override;
  end;

implementation

{$R 'dxRichEdit.Control.Images.RES'}

uses
  RTLConsts, Math,
  dxTypeHelpers,
  dxSpellCheckerCore,
  dxRichEdit.Utils.Exceptions,
  dxRichEdit.Utils.Graphics,
  dxRichEdit.DocumentModel.VisibleTextFilter.Core,
  dxRichEdit.Commands.CopyAndPaste,
  dxRichEdit.Commands.Tables,
  dxRichEdit.Commands.Tables.Cells,
  dxRichEdit.Commands.SpellChecker,
  dxRichEdit.Utils.Types,
  dxRichEdit.Commands.FloatingObject;

{ TdxRichEditPopupMenuManager }

class procedure TdxRichEditPopupMenuImagesManager.Finalize;
begin
  FreeAndNil(FImages);
end;

class procedure TdxRichEditPopupMenuImagesManager.AddImagesFromResource(
  const AResourceName: string; AImages: TcxImageList);
var
  AColCount, AColIndex: Integer;
  ADestBitmap: TcxBitmap;
  ASrcPoint: TPoint;
  AImage: TBitmap;
  AResourceImage: TdxSmartImage;
begin
  AResourceImage := CreateImageFromResource(HInstance, AResourceName);
  try
    AImage := AResourceImage.GetAsBitmap;
    try
      AColCount := AImage.Width div AImages.Width;
      ADestBitmap := TcxBitmap.CreateSize(AImages.Width, AImages.Height, pf32bit);
      try
        AImages.BeginUpdate;
        try
          for AColIndex := 0 to AColCount - 1 do
          begin
            ASrcPoint := Point(AColIndex * AImages.Width, 0);

            ADestBitmap.Canvas.Brush.Color := clBlack;
            ADestBitmap.Canvas.FillRect(ADestBitmap.ClientRect);

            cxDrawBitmap(ADestBitmap.Canvas.Handle, AImage, ADestBitmap.ClientRect, ASrcPoint);
            AImages.AddMasked(ADestBitmap, clNone);
          end;
        finally
          AImages.EndUpdate;
        end;
      finally
        ADestBitmap.Free;
      end;
    finally
      AImage.Free;
    end;
  finally
    AResourceImage.Free;
  end;
end;

class function TdxRichEditPopupMenuImagesManager.CreateImageList: TcxImageList;
begin
  Result := TcxImageList.CreateSize(16, 16);
end;

class function TdxRichEditPopupMenuImagesManager.GetImageIndex(ACommandID: TdxRichEditCommandID): Integer;
const
  Indexes: array[0..46] of TdxRichEditCommandId = (
    TdxRichEditCommandId.UpdateField,
    TdxRichEditCommandId.ToggleFieldCodes,
    TdxRichEditCommandId.CutSelection,
    TdxRichEditCommandId.CopySelection,
    TdxRichEditCommandId.PasteSelection,
    TdxRichEditCommandId.ToggleTableAutoFitContents,
    TdxRichEditCommandId.ToggleTableAutoFitWindow,
    TdxRichEditCommandId.ToggleTableFixedColumnWidth,
    TdxRichEditCommandId.MergeTableElement,
    TdxRichEditCommandId.ShowSplitTableCellsFormMenuItem,
    TdxRichEditCommandId.ShowTablePropertiesFormMenuItem,
    TdxRichEditCommandId.InsertTableColumnToTheLeft,
    TdxRichEditCommandId.InsertTableColumnToTheRight,
    TdxRichEditCommandId.InsertTableRowAbove,
    TdxRichEditCommandId.InsertTableRowBelow,
    TdxRichEditCommandId.ShowInsertTableCellsForm,
    TdxRichEditCommandId.ToggleTableCellsTopLeftAlignment,
    TdxRichEditCommandId.ToggleTableCellsTopCenterAlignment,
    TdxRichEditCommandId.ToggleTableCellsTopRightAlignment,
    TdxRichEditCommandId.ToggleTableCellsMiddleLeftAlignment,
    TdxRichEditCommandId.ToggleTableCellsMiddleCenterAlignment,
    TdxRichEditCommandId.ToggleTableCellsMiddleRightAlignment,
    TdxRichEditCommandId.ToggleTableCellsBottomLeftAlignment,
    TdxRichEditCommandId.ToggleTableCellsBottomCenterAlignment,
    TdxRichEditCommandId.ToggleTableCellsBottomRightAlignment,
    TdxRichEditCommandId.IncreaseIndent,
    TdxRichEditCommandId.DecreaseIndent,
    TdxRichEditCommandId.ShowFontForm,
    TdxRichEditCommandId.ShowParagraphForm,
    TdxRichEditCommandId.ShowNumberingListForm,
    TdxRichEditCommandId.ShowFloatingObjectLayoutOptionsForm,
    TdxRichEditCommandId.CreateBookmark,
    TdxRichEditCommandId.CreateHyperlink,
    TdxRichEditCommandId.EditHyperlink,
    TdxRichEditCommandId.RemoveHyperlink,
    TdxRichEditCommandId.SetFloatingObjectSquareTextWrapType,
    TdxRichEditCommandId.SetFloatingObjectTightTextWrapType,
    TdxRichEditCommandId.SetFloatingObjectThroughTextWrapType,
    TdxRichEditCommandId.SetFloatingObjectTopAndBottomTextWrapType,
    TdxRichEditCommandId.SetFloatingObjectBehindTextWrapType,
    TdxRichEditCommandId.SetFloatingObjectInFrontOfTextWrapType,
    TdxRichEditCommandId.FloatingObjectBringToFront,
    TdxRichEditCommandId.FloatingObjectBringForward,
    TdxRichEditCommandId.FloatingObjectBringInFrontOfText,
    TdxRichEditCommandId.FloatingObjectSendToBack,
    TdxRichEditCommandId.FloatingObjectSendBackward,
    TdxRichEditCommandId.FloatingObjectSendBehindText
  );
var
  AImageIndex: Integer;
begin
  Result := -1;
  for AImageIndex := Low(Indexes) to High(Indexes) do
    if Indexes[AImageIndex] = ACommandID then
      Exit(AImageIndex);
end;

class procedure TdxRichEditPopupMenuImagesManager.PopulateImages(AImages: TcxImageList);
begin
  AddImagesFromResource('DXRECPMIMAGES', AImages);
end;

class function TdxRichEditPopupMenuImagesManager.GetImages: TcxImageList;
begin
  if FImages = nil then
  begin
    FImages := CreateImageList;
    PopulateImages(FImages);
  end;
  Result := FImages;
end;

{ TdxRichEditCustomPopupMenu }

constructor TdxRichEditCustomPopupMenu.Create(AControl: TdxVCLControl);
begin
  inherited Create(nil);
  FControl := AControl;
  FCommands := TdxFastObjectList.Create(True, 32);
  FAdapter := TdxBuiltInPopupMenuAdapterManager.GetActualAdapterClass.Create(Self);
  FAdapter.SetImages(GetManagerClass.Images);
end;

destructor TdxRichEditCustomPopupMenu.Destroy;
begin
  FreeAndNil(FAdapter);
  FreeAndNil(FCommands);
  inherited Destroy;
end;

class function TdxRichEditCustomPopupMenu.GetManagerClass: TdxRichEditPopupMenuImagesManagerClass;
begin
  Result := TdxRichEditPopupMenuImagesManager;
end;

function TdxRichEditCustomPopupMenu.AddMenuItem(ACommandID: TdxRichEditCommandID; AParent: TComponent = nil): TComponent;
begin
  Result := AddCommand(ACommandID, False, AParent);
end;

function TdxRichEditCustomPopupMenu.AddMenuCheckItem(ACommandID: TdxRichEditCommandID; AParent: TComponent): TComponent;
begin
  Result := AddCommand(ACommandID, False, AParent, True);
end;

function TdxRichEditCustomPopupMenu.AddSubMenu(ACommandID: TdxRichEditCommandID; AParent: TComponent = nil): TComponent;
begin
  Result := AddCommand(ACommandID, True, AParent);
end;

function TdxRichEditCustomPopupMenu.AddCommand(ACommand: TdxRichEditCommand; AIsSubMenu: Boolean; AParent: TComponent;
  AIsCheckItem: Boolean = False): TComponent;
var
  AState: IdxCommandUIState;
  AIndex, AImageIndex: Integer;
begin
  Result := nil;
  if ACommand = nil then
    Exit;
  AIndex := FCommands.Add(ACommand);
  AState := ACommand.CreateDefaultCommandUIState;
  ACommand.UpdateUIState(AState);
  if AState.Visible then
  begin
    AImageIndex := GetManagerClass.GetImageIndex(ACommand.Id);
    if AIsSubMenu then
      Result := Adapter.AddSubMenu(ACommand.GetObjectMenuCaption, MenuItemClick, AIndex, AImageIndex, AState.Enabled, 0, AParent)
    else
      Result := Adapter.Add(ACommand.GetObjectMenuCaption, MenuItemClick, AIndex, AImageIndex, AState.Enabled, 0, AParent);
    if AIsCheckItem then
      Adapter.SetChecked(Result, AState.Checked);
  end;
end;

function TdxRichEditCustomPopupMenu.AddCommand(ACommandID: TdxRichEditCommandID; AIsSubMenu: Boolean;
  AParent: TComponent; AIsCheckItem: Boolean = False): TComponent;
begin
  Result := AddCommand(InnerControl.CreateCommand(ACommandID), AIsSubMenu, AParent, AIsCheckItem);
end;

procedure TdxRichEditCustomPopupMenu.Initialize;
begin
  Adapter.Clear;
  FCommands.Clear;
  Adapter.SetLookAndFeel(Control.LookAndFeel);
  PopulateMenuItems;
end;

function TdxRichEditCustomPopupMenu.Popup(const P: TPoint): Boolean;
begin
  Initialize;
  Result := Adapter.Popup(Control.ClientToScreen(P));
end;

procedure TdxRichEditCustomPopupMenu.MenuItemClick(Sender: TObject);
begin
  ProcessClick(TdxRichEditCommand(FCommands[TComponent(Sender).Tag]));
end;

procedure TdxRichEditCustomPopupMenu.ProcessClick(ACommand: TdxRichEditCommand);
begin
  ACommand.Execute;
end;

function TdxRichEditCustomPopupMenu.GetInnerControl: IdxInnerControl;
begin
  Result := RichEditControl.InnerControl;
end;

function TdxRichEditCustomPopupMenu.GetRichEditControl: IdxRichEditControl;
begin
  Result := Control as IdxRichEditControl;
end;

{ TdxRichEditContentPopupMenu }

function TdxRichEditContentPopupMenu.AddFieldMenu: Boolean;
var
  AInnerControl: IdxInnerControl;
  AField: TdxField;
begin
  AInnerControl := RichEditControl.InnerControl;

  AField := GetSelectedField;
  while (AField <> nil) and AField.HideByParent do
    AField := AField.Parent;

  if AField = nil then
    Exit(False);

  if AInnerControl.DocumentModel.ActivePieceTable.IsHyperlinkField(AField) then
    AddHyperlinkMenuItems
  else
    AddFieldMenuItems;
  Result := True;
end;

procedure TdxRichEditContentPopupMenu.AddClipboardMenuItems;
begin
  AddMenuItem(TdxRichEditCommandId.CutSelection);
  AddMenuItem(TdxRichEditCommandId.CopySelection);
  AddMenuItem(TdxRichEditCommandId.PasteSelection);
end;

procedure TdxRichEditContentPopupMenu.AddFieldMenuItems;
begin
  AddMenuItem(TdxRichEditCommandId.UpdateField);
  AddMenuItem(TdxRichEditCommandId.ToggleFieldCodes);
end;

procedure TdxRichEditContentPopupMenu.AddFloatingObjectTextWrapTypeSubmenu;
var
  ASubmenu: TComponent;
begin
  ASubmenu := AddCommand(TdxChangeFloatingObjectTextWrapTypeMenuCommand.Create(RichEditControl), True, nil);
  if ASubmenu = nil then
    Exit;
  AddMenuItem(TdxRichEditCommandId.SetFloatingObjectSquareTextWrapType, ASubmenu);
  AddMenuItem(TdxRichEditCommandId.SetFloatingObjectTightTextWrapType, ASubmenu);
  AddMenuItem(TdxRichEditCommandId.SetFloatingObjectThroughTextWrapType, ASubmenu);
  AddMenuItem(TdxRichEditCommandId.SetFloatingObjectTopAndBottomTextWrapType, ASubmenu);
  AddMenuItem(TdxRichEditCommandId.SetFloatingObjectBehindTextWrapType, ASubmenu);
  AddMenuItem(TdxRichEditCommandId.SetFloatingObjectInFrontOfTextWrapType, ASubmenu);
end;

procedure TdxRichEditContentPopupMenu.AddFloatingObjectBringForwardSubmenu;
var
  ASubmenu: TComponent;
begin
  ASubmenu := AddCommand(TdxFloatingObjectBringForwardMenuCommand.Create(RichEditControl), True, nil);
  if ASubmenu = nil then
    Exit;
  AddMenuItem(TdxRichEditCommandId.FloatingObjectBringToFront, ASubmenu);
  AddMenuItem(TdxRichEditCommandId.FloatingObjectBringForward, ASubmenu);
  AddMenuItem(TdxRichEditCommandId.FloatingObjectBringInFrontOfText, ASubmenu);
end;

procedure TdxRichEditContentPopupMenu.AddFloatingObjectSendBackwardSubmenu;
var
  ASubmenu: TComponent;
begin
  ASubmenu := AddCommand(TdxFloatingObjectSendBackwardMenuCommand.Create(RichEditControl), True, nil);
  if ASubmenu = nil then
    Exit;
  AddMenuItem(TdxRichEditCommandId.FloatingObjectSendToBack, ASubmenu);
  AddMenuItem(TdxRichEditCommandId.FloatingObjectSendBackward, ASubmenu);
  AddMenuItem(TdxRichEditCommandId.FloatingObjectSendBehindText, ASubmenu);
end;

procedure TdxRichEditContentPopupMenu.AddHyperlinkMenuItems;
begin
  AddMenuItem(TdxRichEditCommandId.EditHyperlink);
  AddMenuItem(TdxRichEditCommandId.OpenHyperlink);
  AddMenuItem(TdxRichEditCommandId.RemoveHyperlink);
end;

procedure TdxRichEditContentPopupMenu.AddSpellCheckerMenuItems;

  function GetWord(const AStart, AEnd: TdxDocumentModelPosition): string;
  var
    AFilter: TdxVisibleTextFilterBase;
  begin
    Assert(AStart.PieceTable = AEnd.PieceTable);
    AFilter := TdxPieceTable(AStart.PieceTable).VisibleTextFilter;
    Result := TdxPieceTable(AStart.PieceTable).GetFilteredPlainText(AStart, AEnd, AFilter.IsRunVisible);
  end;

  function GetSpellingSuggestions(const AWord: string; const AStart, AEnd: TdxDocumentModelPosition): TArray<string>;
  var
    ASpellChecker: IdxSpellChecker3;
  begin
    ASpellChecker := TdxSpellCheckerInstance.ISpellChecker3;
    Assert(ASpellChecker <> nil);
    Result := ASpellChecker.GetSuggestions(AWord);
  end;

  procedure DoPopulateSuggestionCommands(AParent: TComponent; ACommandClass: TdxReplaceMisspellingCommandClass;
    const AWord: string; const AStart, AEnd: TdxDocumentModelPosition);
  var
    ASuggestions: TArray<string>;
    ACount, I: Integer;
    ACommand: TdxRichEditCommand;
  begin
    ASuggestions := GetSpellingSuggestions(AWord, AStart, AEnd);
    ACount := Min(Length(ASuggestions), TdxSpellCheckerInstance.ISpellChecker3.CheckAsYouTypeOptions.SuggestionCount);
    if ACount > 0 then
    begin
      for I := 0 to ACount - 1 do
      begin
        ACommand := ACommandClass.Create(RichEditControl, ASuggestions[I]);
        if I = 0 then
          Adapter.AddSeparator;
        AddCommand(ACommand, False, AParent);
      end;
    end
    else
    begin
      ACommand := TdxReplaceMisspellingCommand.Create(RichEditControl, '');
      Adapter.AddSeparator;
      AddCommand(ACommand, False, AParent);
    end;
  end;

  procedure PopulateSuggestionCommands(const AWord: string; const AStart, AEnd: TdxDocumentModelPosition);
  begin
    DoPopulateSuggestionCommands(nil, TdxReplaceMisspellingCommand, AWord, AStart, AEnd);
  end;

  procedure PopulateAutoCorrectSuggestionCommands(const AWord: string; const AStart, AEnd: TdxDocumentModelPosition);
  var
    AParent: TComponent;
  begin
    AParent := AddCommand(TdxAutoCorrectPlaceholderMenuCommand.Create(RichEditControl), True, nil);
    DoPopulateSuggestionCommands(AParent, TdxAutoCorrectMisspellingCommand, AWord, AStart, AEnd);
  end;

var
  APieceTable: TdxPieceTable;
  ALogPosition: TdxDocumentLogPosition;
  AIntervals: TdxMisspelledIntervalCollection;
  AInterval: TdxMisspelledInterval;
  AStart, AEnd: TdxDocumentModelPosition;
  AWord: string;
  ACheckAsYouTypeOptions: TdxSpellCheckerCustomCheckAsYouTypeOptions;
begin
  if TdxSpellCheckerInstance.ISpellChecker3 = nil then
    Exit;
  ACheckAsYouTypeOptions := TdxSpellCheckerInstance.ISpellChecker3.CheckAsYouTypeOptions;
  APieceTable := RichEditControl.InnerControl.DocumentModel.ActivePieceTable;
  ALogPosition := APieceTable.DocumentModel.Selection.Interval.NormalizedStart.LogPosition;
  AIntervals := APieceTable.SpellCheckerManager.MisspelledIntervals;
  AInterval := AIntervals.FindInterval(ALogPosition);
  if AInterval = nil then
    Exit;

  if AInterval.ErrorType = seMisspelling then
  begin
    AStart := AInterval.Interval.Start;
    AEnd := AInterval.Interval.&End;
    AWord := GetWord(AStart, AEnd);
    if scmiSuggestions in ACheckAsYouTypeOptions.PopupMenuItems then
      PopulateSuggestionCommands(AWord, AStart, AEnd);
    Adapter.AddSeparator;
    if scmiIgnore in ACheckAsYouTypeOptions.PopupMenuItems then
      AddMenuItem(TdxRichEditCommandId.IgnoreMisspelling);
    if scmiIgnoreAll in ACheckAsYouTypeOptions.PopupMenuItems then
      AddMenuItem(TdxRichEditCommandId.IgnoreAllMisspellings);
    if scmiAddToDictionary in ACheckAsYouTypeOptions.PopupMenuItems then
      AddMenuItem(TdxRichEditCommandId.AddWordToDictionary);
  end
  else
  begin
    if scmiDelete in ACheckAsYouTypeOptions.PopupMenuItems then
      AddMenuItem(TdxRichEditCommandId.DeleteRepeatedWord);
    if scmiIgnore in ACheckAsYouTypeOptions.PopupMenuItems then
      AddMenuItem(TdxRichEditCommandId.IgnoreMisspelling);
  end;
  Adapter.AddSeparator;
  if (AInterval.ErrorType = seMisspelling) and
      (scmiAutoCorrect in ACheckAsYouTypeOptions.PopupMenuItems) then
    PopulateAutoCorrectSuggestionCommands(AWord, AStart, AEnd);
  if scmiSpelling in ACheckAsYouTypeOptions.PopupMenuItems then
  begin
    AddMenuItem(TdxRichEditCommandId.CheckSpelling);
    Adapter.AddSeparator;
  end;
end;

procedure TdxRichEditContentPopupMenu.AddTableAutoFitSubmenu;
var
  ASubmenu: TComponent;
begin
  ASubmenu := AddCommand(TdxToggleTableAutoFitPlaceholderMenuCommand.Create(RichEditControl), True, nil);
  if ASubmenu = nil then
    Exit;
  AddMenuItem(TdxRichEditCommandId.ToggleTableAutoFitContents, ASubmenu);
  AddMenuItem(TdxRichEditCommandId.ToggleTableAutoFitWindow, ASubmenu);
  AddMenuItem(TdxRichEditCommandId.ToggleTableFixedColumnWidth, ASubmenu);
end;

procedure TdxRichEditContentPopupMenu.AddTableOptionsMenuItems;
begin
  AddTableInsertSubmenu;
  AddMenuItem(TdxRichEditCommandId.DeleteTableRowsMenuItem);
  AddMenuItem(TdxRichEditCommandId.DeleteTableColumnsMenuItem);
  AddMenuItem(TdxRichEditCommandId.ShowDeleteTableCellsFormMenuItem);
  AddMenuItem(TdxRichEditCommandId.MergeTableElement);
  AddMenuItem(TdxRichEditCommandId.ShowSplitTableCellsFormMenuItem);
  AddTableCellsAlignmentSubmenu;
  AddTableAutoFitSubmenu;
  AddMenuItem(TdxRichEditCommandId.ShowTablePropertiesFormMenuItem);
end;

procedure TdxRichEditContentPopupMenu.AddTableCellsAlignmentSubmenu;
var
  ASubmenu: TComponent;
begin
  ASubmenu := AddCommand(TdxChangeTableCellsContentAlignmentPlaceholderCommand.Create(RichEditControl), True, nil);
  if ASubmenu = nil then
    Exit;
  AddMenuCheckItem(TdxRichEditCommandId.ToggleTableCellsTopLeftAlignment, ASubmenu);
  AddMenuCheckItem(TdxRichEditCommandId.ToggleTableCellsTopCenterAlignment, ASubmenu);
  AddMenuCheckItem(TdxRichEditCommandId.ToggleTableCellsTopRightAlignment, ASubmenu);
  Adapter.AddSeparator;
  AddMenuCheckItem(TdxRichEditCommandId.ToggleTableCellsMiddleLeftAlignment, ASubmenu);
  AddMenuCheckItem(TdxRichEditCommandId.ToggleTableCellsMiddleCenterAlignment, ASubmenu);
  AddMenuCheckItem(TdxRichEditCommandId.ToggleTableCellsMiddleRightAlignment, ASubmenu);
  Adapter.AddSeparator;
  AddMenuCheckItem(TdxRichEditCommandId.ToggleTableCellsBottomLeftAlignment, ASubmenu);
  AddMenuCheckItem(TdxRichEditCommandId.ToggleTableCellsBottomCenterAlignment, ASubmenu);
  AddMenuCheckItem(TdxRichEditCommandId.ToggleTableCellsBottomRightAlignment, ASubmenu);
end;

procedure TdxRichEditContentPopupMenu.AddTableInsertSubmenu;
var
  ASubmenu: TComponent;
begin
  ASubmenu := AddCommand(TdxInsertTableElementMenuCommand.Create(RichEditControl), True, nil);
  if ASubmenu = nil then
    Exit;
  AddMenuItem(TdxRichEditCommandId.InsertTableColumnToTheLeft, ASubmenu);
  AddMenuItem(TdxRichEditCommandId.InsertTableColumnToTheRight, ASubmenu);
  AddMenuItem(TdxRichEditCommandId.InsertTableRowAbove, ASubmenu);
  AddMenuItem(TdxRichEditCommandId.InsertTableRowBelow, ASubmenu);
  AddMenuItem(TdxRichEditCommandId.ShowInsertTableCellsForm, ASubmenu);
end;

function TdxRichEditContentPopupMenu.GetSelectedField: TdxField;
var
  ASelection: TdxSelection;
  ASelectionInterval: TdxRunInfo;
  ASelectionStart, ASelectionEnd: PdxDocumentModelPosition;
  APieceTable: TdxPieceTable;
  AFirstFieldIndex, ALastFieldIndex, I: Integer;
  ACurrent, ATopMostField: TdxField;
begin
  ASelection := RichEditControl.InnerControl.DocumentModel.Selection;
  ASelectionInterval := ASelection.Interval;
  ASelectionStart := ASelectionInterval.NormalizedStart;
  ASelectionEnd := ASelectionInterval.NormalizedEnd;

  APieceTable := ASelection.PieceTable;

  AFirstFieldIndex := APieceTable.FindFieldIndexByRunIndex(ASelectionStart.RunIndex);
  ALastFieldIndex := APieceTable.FindFieldIndexByRunIndex(ASelectionEnd.RunIndex);

  if (AFirstFieldIndex = ALastFieldIndex) and (AFirstFieldIndex >= 0) then
    Exit(APieceTable.Fields[AFirstFieldIndex]);

  if AFirstFieldIndex < 0 then
    AFirstFieldIndex := not AFirstFieldIndex;

  if ALastFieldIndex < 0 then
    ALastFieldIndex := not ALastFieldIndex;

  if AFirstFieldIndex = ALastFieldIndex then
    Exit(GetInvisibleField(ASelectionStart^));

  if AFirstFieldIndex = ALastFieldIndex - 1 then
    Exit(APieceTable.Fields[AFirstFieldIndex]);

  Result := nil;
  for I := AFirstFieldIndex to ALastFieldIndex - 1 do
  begin
    ACurrent := APieceTable.Fields[I];
    ATopMostField := ACurrent.GetTopLevelParent;
    if ATopMostField = nil then
      ATopMostField := ACurrent;
    if Result = nil then
      Result := ATopMostField
    else
      if Result <> ATopMostField then
        Exit(nil);
  end;
end;

function TdxRichEditContentPopupMenu.GetInvisibleField(const ASelectionStart: TdxDocumentModelPosition): TdxField;
var
  ARunIndex: TdxRunIndex;
  APieceTable: TdxPieceTable;
  AHiddenFieldIndex: Integer;
begin
  ARunIndex := ASelectionStart.RunIndex;
  if (ASelectionStart.RunOffset > 0) or (ARunIndex = 0) then
    Exit(nil);

  APieceTable := TdxPieceTable(ASelectionStart.PieceTable);
  repeat
    Dec(ARunIndex);
  until not ((ARunIndex > 0) and not APieceTable.VisibleTextFilter.IsRunVisible(ARunIndex));

  if APieceTable.VisibleTextFilter.IsRunVisible(ARunIndex) then
    Inc(ARunIndex);

  if ARunIndex = ASelectionStart.RunIndex then
    Exit(nil);

  AHiddenFieldIndex := APieceTable.FindFieldIndexByRunIndex(ARunIndex);
  if (AHiddenFieldIndex >= 0) and (APieceTable.Fields[AHiddenFieldIndex].FirstRunIndex = ARunIndex) then
    Result := APieceTable.Fields[AHiddenFieldIndex]
  else
    Result := nil;
end;

procedure TdxRichEditContentPopupMenu.PopulateMenuItems;
var
  AHasFieldMenu: Boolean;
begin
  AHasFieldMenu := AddFieldMenu;
  if AHasFieldMenu then
    Adapter.AddSeparator;
  AddSpellCheckerMenuItems;
  AddClipboardMenuItems;
  Adapter.AddSeparator;
  AddTableOptionsMenuItems;
  Adapter.AddSeparator;
  AddMenuItem(TdxRichEditCommandId.IncreaseIndent);
  AddMenuItem(TdxRichEditCommandId.DecreaseIndent);
  Adapter.AddSeparator;
  AddMenuItem(TdxRichEditCommandId.ShowFontForm);
  AddMenuItem(TdxRichEditCommandId.ShowParagraphForm);
  AddMenuItem(TdxRichEditCommandId.ShowNumberingListForm);
  Adapter.AddSeparator;
  AddMenuItem(TdxRichEditCommandId.CreateBookmark);
  if not AHasFieldMenu then
    AddMenuItem(TdxRichEditCommandId.CreateHyperlink)
  else
    AddMenuItem(TdxRichEditCommandId.ShowTOCForm);
  AddFloatingObjectTextWrapTypeSubMenu;
  AddFloatingObjectBringForwardSubMenu;
  AddFloatingObjectSendBackwardSubMenu;
  AddMenuItem(TdxRichEditCommandId.ShowFloatingObjectLayoutOptionsForm);
end;

procedure Finalize;
begin
  TdxRichEditPopupMenuImagesManager.Finalize;
end;

initialization
  dxUnitsLoader.AddUnit(nil, @Finalize);

finalization
  dxUnitsLoader.RemoveUnit(@Finalize);

end.
