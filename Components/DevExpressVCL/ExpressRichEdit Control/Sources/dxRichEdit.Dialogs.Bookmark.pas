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

unit dxRichEdit.Dialogs.Bookmark;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Menus, StdCtrls, Generics.Defaults, Generics.Collections,
  Controls, Forms, Dialogs, dxCore, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxContainer, cxEdit, dxLayoutcxEditAdapters, dxLayoutContainer, cxListBox, cxTextEdit,
  dxLayoutControl, dxLayoutControlAdapters, cxButtons, cxRadioGroup, cxLabel,
  dxLayoutLookAndFeels, cxClasses,
  dxRichEdit.Dialogs.CustomDialog,
  dxRichEdit.Dialogs.BookmarkFormController,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.Dialogs.ControlHelper,
  dxRichEdit.DocumentModel.Bookmarks,
  dxRichEdit.DocumentModel.PieceTable, cxCustomListBox;

type

  { TdxRichEditBookmarkNameHelper }

  TdxRichEditBookmarkNameHelper = class(TdxCustomListBoxHelper)
  protected
    procedure OnLastEndUpdate; override;
  end;

  { TdxRichEditBookmarkDialogForm }

  TdxRichEditBookmarkDialogForm = class(TdxRichEditCustomDialogForm)
    btnAdd: TcxButton;
    btnCancel: TcxButton;
    btnDelete: TcxButton;
    btnGoTo: TcxButton;
    dxLayoutControl1Group1: TdxLayoutAutoCreatedGroup;
    dxLayoutControl1Group2: TdxLayoutAutoCreatedGroup;
    dxLayoutControl1Group3: TdxLayoutGroup;
    dxLayoutControl1Item10: TdxLayoutItem;
    dxLayoutControl1Item2: TdxLayoutItem;
    dxLayoutControl1Item4: TdxLayoutItem;
    dxLayoutControl1Item5: TdxLayoutItem;
    dxLayoutControl1Item6: TdxLayoutItem;
    dxLayoutControl1Item7: TdxLayoutItem;
    dxLayoutControl1Item8: TdxLayoutItem;
    dxLayoutCxLookAndFeel2: TdxLayoutCxLookAndFeel;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    edtBookmarkName: TcxTextEdit;
    lbBookmarkName: TcxListBox;
    lblSortBy: TdxLayoutLabeledItem;
    lciBookmarkName: TdxLayoutItem;
    lcMainGroup_Root: TdxLayoutGroup;
    rbSortByLocation: TcxRadioButton;
    rbSortByName: TcxRadioButton;
    procedure rbSortByClick(Sender: TObject);
    procedure btnGoToClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure edtBookmarkNamePropertiesChange(Sender: TObject);
    procedure lbBookmarkNameDblClick(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
      var Handled: Boolean);
    procedure lbBookmarkNameClick(Sender: TObject);
  private
    FBookmarkNameHelper: TdxRichEditBookmarkNameHelper;
    function GetController: TdxBookmarkFormController; inline;
  protected
    procedure ApplyLocalization; override;
    function CreateController(AControllerParameters: TdxFormControllerParameters): TdxFormController; override;
    procedure UpdateFormCore; override;
    procedure SubscribeControlsEvents; override;
    procedure UnsubscribeControlsEvents; override;

    procedure InitializeBookmarkNameEdit;
    procedure UpdateAddButtonEnabled;
    procedure UpdateBookmarkNameItems(const ABookmarks: TdxBookmarkList);
    procedure UpdateButtonsEnabled;
    procedure SelectBookmark;
    function ShowWarningDialog: Boolean;
    procedure InitializeForm; override;
  public
    destructor Destroy; override;
    property Controller: TdxBookmarkFormController read GetController;
  end;

implementation

uses
  Contnrs,
  dxRichEdit.Dialogs.Strs,
  dxStringHelper,
  dxRichEdit.Utils.Exceptions.Strs;

{$R *.dfm}

{ TdxRichEditBookmarkDialogForm }

function TdxRichEditBookmarkDialogForm.GetController: TdxBookmarkFormController;
begin
  Result := TdxBookmarkFormController(inherited Controller);
end;

procedure TdxRichEditBookmarkDialogForm.InitializeBookmarkNameEdit;
begin
  if lbBookmarkName.ItemIndex >= 0 then
    edtBookmarkName.Text := TdxBookmark(lbBookmarkName.ItemObject).Name
  else
    edtBookmarkName.Text := '';
  UpdateAddButtonEnabled;
end;

procedure TdxRichEditBookmarkDialogForm.InitializeForm;
begin
  FBookmarkNameHelper := TdxRichEditBookmarkNameHelper.Create(edtBookmarkName, lbBookmarkName);
  FBookmarkNameHelper.OnChange := edtBookmarkNamePropertiesChange;
end;

procedure TdxRichEditBookmarkDialogForm.lbBookmarkNameClick(Sender: TObject);
var
  AItem: TObject;
begin
  AItem := lbBookmarkName.ItemObject;
  if AItem <> nil then
    edtBookmarkName.EditValue := TdxBookmark(AItem).Name
  else
    edtBookmarkName.EditValue := '';
  if Visible then
    edtBookmarkName.SetFocus;
  UpdateButtonsEnabled;
end;

procedure TdxRichEditBookmarkDialogForm.lbBookmarkNameDblClick(Sender: TObject);
begin
  if lbBookmarkName.ItemIndex >= 0 then
    SelectBookmark;
end;

procedure TdxRichEditBookmarkDialogForm.rbSortByClick(Sender: TObject);
var
  ABookmarks: TdxBookmarkList;
begin
  if rbSortByLocation.Checked then
    ABookmarks := Controller.GetBookmarksSortedByLocation(False)
  else
    ABookmarks := Controller.GetBookmarksSortedByName(False);
  try
    UpdateBookmarkNameItems(ABookmarks);
  finally
    ABookmarks.Free;
  end;
end;

procedure TdxRichEditBookmarkDialogForm.SelectBookmark;
var
  ABookmark: TdxBookmark;
begin
  ABookmark := lbBookmarkName.ItemObject as TdxBookmark;
  Controller.SelectBookmark(ABookmark);
end;

function TdxRichEditBookmarkDialogForm.ShowWarningDialog: Boolean;
var
  AText: string;
begin
  AText := cxGetResourceString(@sdxRichEditExceptionBookmarkCreationFailing);
  Result := Application.MessageBox(PChar(AText), PChar(Application.Title), MB_YESNO + MB_ICONWARNING) = ID_YES;
end;

procedure TdxRichEditBookmarkDialogForm.SubscribeControlsEvents;
begin
  lbBookmarkName.OnClick := lbBookmarkNameClick;
  edtBookmarkName.Properties.OnChange := edtBookmarkNamePropertiesChange;
end;

procedure TdxRichEditBookmarkDialogForm.UnsubscribeControlsEvents;
begin
  lbBookmarkName.OnClick := nil;
  edtBookmarkName.Properties.OnChange := nil;
end;

procedure TdxRichEditBookmarkDialogForm.UpdateAddButtonEnabled;
var
  ABookmarkName: string;
begin
  ABookmarkName := edtBookmarkName.Text;
  btnAdd.Enabled := (ABookmarkName <> '') and Controller.ValidateName(ABookmarkName);
end;

procedure TdxRichEditBookmarkDialogForm.UpdateFormCore;
begin
  rbSortByClick(Self);
  InitializeBookmarkNameEdit;
end;

procedure TdxRichEditBookmarkDialogForm.UpdateBookmarkNameItems(const ABookmarks: TdxBookmarkList);
var
  AItems: TStrings;
  ASelectedItem, ABookmark: TdxBookmark;
  I: Integer;
begin
  FBookmarkNameHelper.BeginUpdate;
  try
    AItems := lbBookmarkName.Items;
    AItems.BeginUpdate;
    try
      ASelectedItem := TdxBookmark(lbBookmarkName.ItemObject);
      AItems.Clear;
      for I := 0 to ABookmarks.Count - 1 do
      begin
        ABookmark := ABookmarks[I];
        AItems.AddObject(ABookmark.Name, ABookmark);
      end;
    finally
      AItems.EndUpdate;
    end;
    if ABookmarks.Count > 0 then
    begin
      if ASelectedItem <> nil then
        lbBookmarkName.ItemIndex := ABookmarks.IndexOf(ASelectedItem)
      else
      begin
        ABookmark := Controller.GetCurrentBookmark;
        lbBookmarkName.ItemIndex := ABookmarks.IndexOf(ABookmark);
        lbBookmarkNameClick(lbBookmarkName);
      end;
    end;
    UpdateButtonsEnabled;
  finally
    FBookmarkNameHelper.EndUpdate;
  end;
end;

procedure TdxRichEditBookmarkDialogForm.UpdateButtonsEnabled;
begin
  if lbBookmarkName.ItemIndex = -1 then
  begin
    btnDelete.Enabled := False;
    btnGoTo.Enabled := False;
  end
  else
  begin
    btnDelete.Enabled := True;
    btnGoTo.Enabled := Controller.CanSelectBookmark(TdxBookmark(lbBookmarkName.ItemObject));
  end;
end;

procedure TdxRichEditBookmarkDialogForm.ApplyLocalization;
begin
  Caption := cxGetResourceString(@sdxRichEditBookmarkDialogForm);
  rbSortByName.Caption := cxGetResourceString(@sdxRichEditBookmarkDialogSortByName);
  rbSortByLocation.Caption := cxGetResourceString(@sdxRichEditBookmarkDialogSortByLocation);
  lblSortBy.Caption := cxGetResourceString(@sdxRichEditBookmarkDialogSortBy);
  btnGoTo.Caption := cxGetResourceString(@sdxRichEditBookmarkDialogButtonGoTo);
  btnCancel.Caption := cxGetResourceString(@sdxRichEditBookmarkDialogButtonCancel);
  btnAdd.Caption := cxGetResourceString(@sdxRichEditBookmarkDialogButtonAdd);
  btnDelete.Caption := cxGetResourceString(@sdxRichEditBookmarkDialogButtonDelete);
  lciBookmarkName.CaptionOptions.Text := cxGetResourceString(@sdxRichEditBookmarkDialogBookmarkName);
end;

procedure TdxRichEditBookmarkDialogForm.btnAddClick(Sender: TObject);
begin
  Controller.CreateBookmark(edtBookmarkName.Text, ShowWarningDialog);
end;

procedure TdxRichEditBookmarkDialogForm.btnDeleteClick(Sender: TObject);
var
  ABookmark: TdxBookmark;
begin
  ABookmark := TdxBookmark(lbBookmarkName.ItemObject);
  Controller.DeleteBookmark(ABookmark);
  UpdateForm;
  edtBookmarkName.SetFocus;
end;

procedure TdxRichEditBookmarkDialogForm.btnGoToClick(Sender: TObject);
begin
  SelectBookmark;
end;

function TdxRichEditBookmarkDialogForm.CreateController(
  AControllerParameters: TdxFormControllerParameters): TdxFormController;
begin
  Result := TdxBookmarkFormController.Create(AControllerParameters as TdxBookmarkFormControllerParameters);
end;

destructor TdxRichEditBookmarkDialogForm.Destroy;
begin
  FreeAndNil(FBookmarkNameHelper);
  inherited Destroy;
end;

procedure TdxRichEditBookmarkDialogForm.edtBookmarkNamePropertiesChange(Sender: TObject);
begin
  UpdateAddButtonEnabled;
end;

procedure TdxRichEditBookmarkDialogForm.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta < 0 then
    lbBookmarkName.TopIndex := lbBookmarkName.TopIndex + 1
  else
    lbBookmarkName.TopIndex := lbBookmarkName.TopIndex - 1;
end;

{ TdxRichEditBookmarkNameHelper }

procedure TdxRichEditBookmarkNameHelper.OnLastEndUpdate;
begin
  SubscribeControlsEvents;
end;

end.
