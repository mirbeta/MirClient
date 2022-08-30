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

unit dxRichEdit.Dialogs.Numbering;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Menus, StdCtrls,
  Controls, Forms, Dialogs, dxCore, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxContainer, cxEdit, dxLayoutcxEditAdapters, dxLayoutControlAdapters, dxLayoutLookAndFeels,
  cxButtons, dxLayoutContainer, cxGroupBox, cxRadioGroup, dxLayoutControl, cxListBox, cxClasses, dxContainerListBox,
  dxRichEdit.Dialogs.CustomDialog,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.DocumentModel.Numbering,
  dxRichEdit.DocumentModel.NumberingFormatting,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.Dialogs.NumberingHelper,
  dxRichEdit.Dialogs.NumberingFormController,
  dxRichEdit.Dialogs.CustomNumberingList;

type
  TdxRichEditNumberingListDialogForm = class(TdxRichEditCustomDialogForm, IdxRichEditNumberingListForm)
    btnCancel: TcxButton;
    btnCustomize: TcxButton;
    btnOk: TcxButton;
    dxLayoutControl1Group2: TdxLayoutGroup;
    dxLayoutControl1Group3: TdxLayoutGroup;
    dxLayoutControl1Group4: TdxLayoutGroup;
    dxLayoutControl1Item1: TdxLayoutItem;
    dxLayoutControl1Item2: TdxLayoutItem;
    dxLayoutControl1Item3: TdxLayoutItem;
    dxLayoutControl1Item4: TdxLayoutItem;
    dxLayoutControl1Item5: TdxLayoutItem;
    dxLayoutControl1Item6: TdxLayoutItem;
    dxLayoutControl1Item7: TdxLayoutItem;
    dxLayoutControl1Item8: TdxLayoutItem;
    lbBulleted: TdxContainerListBox;
    lbNumbered: TdxContainerListBox;
    lbOutlineNumbered: TdxContainerListBox;
    lcgBulleted: TdxLayoutGroup;
    lcgNumbered: TdxLayoutGroup;
    lcgOutlineNumbered: TdxLayoutGroup;
    lcgTabControl: TdxLayoutGroup;
    rbContinuePreviousList: TcxRadioButton;
    rbRestartNumbering: TcxRadioButton;
    procedure lcgTabControlTabChanged(Sender: TObject);
    procedure lcgTabControlTabChanging(Sender: TObject; ANewTabIndex: Integer; var Allow: Boolean);
    procedure btnCustomizeClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
  private
    procedure InitializeTabs;
    procedure InitializeNumberingMode;
    { IdxRichEditNumberingListForm }
    function GetController: TdxNumberingListFormController;
    function GetInitialTabPage(AList: TdxAbstractNumberingList): TdxLayoutGroup;
    function GetActiveGallery: TdxNumberingListBoxHelper;
    function GetRestartNumbering: Boolean;
    function GetContinuePreviousList: Boolean;
    procedure SetRestartNumbering(const Value: Boolean);
    procedure SetContinuePreviousList(const Value: Boolean);
  protected
    FNumberingListFormHelper: TdxNumberingListFormHelper;
    FBulletedListBoxHelper: TdxNumberingListBoxHelper;
    FNumberingListBoxHelper: TdxNumberingListBoxHelper;
    FOutlineNumberedListBoxHelper: TdxNumberingListBoxHelper;
    procedure ApplyLocalization; override;
    procedure ApplyChangesWithoutCustomization;
    function ChangeSelectedTabPage(ACurrentSelectedTabPage: TdxLayoutGroup): TdxLayoutGroup;
    function CreateController(AControllerParameters: TdxFormControllerParameters): TdxFormController; override;
    function GetNumberingListType(AList: TdxAbstractNumberingList): TdxNumberingType;
    procedure InitializeForm; override;
    procedure SubscribeGalleryEvents(AGallery: TdxNumberingListBoxHelper);
    procedure UnSubscribeGalleryEvents(AGallery: TdxNumberingListBoxHelper);
    procedure OnActiveGalleryMouseDoubleClick(Sender: TObject);
    procedure OnSelectedIndexChanged(Sender: TObject);

    function CreateNumberingListFormCore(AListLevels: TdxListLevelCollection; AFirstParagraphStartIndex: Integer;
      AMultiLevels: Boolean): TdxRichEditCustomDialogForm;
    { IRichEditNumberingListForm }
    function IsBulletedTabSelected: Boolean;
    function IsNumberedTabSelected: Boolean;
    function IsOutlineNumberedTabSelected: Boolean;
  public
    destructor Destroy; override;
    procedure ShowBulletedListForm(ASource: TdxAbstractNumberingList; ALevels: TdxListLevelCollection);
    procedure ShowSimpleNumberingListForm(ASource: TdxAbstractNumberingList; ALevels: TdxListLevelCollection);
    procedure ShowMultiLevelNumberingListForm(ASource: TdxAbstractNumberingList; ALevels: TdxListLevelCollection);
    property ActiveGallery: TdxNumberingListBoxHelper read GetActiveGallery;
    property Controller: TdxNumberingListFormController read GetController;
    property ContinuePreviousList: Boolean read GetContinuePreviousList write SetContinuePreviousList;
    property RestartNumbering: Boolean read GetRestartNumbering write SetRestartNumbering;
  end;

implementation

uses
  dxRichEdit.Dialogs.Strs,
  dxRichEdit.Dialogs.BulletedList,
  dxRichEdit.Dialogs.SimpleNumberingList,
  dxRichEdit.Dialogs.MultiLevelNumberingList,
  dxRichEdit.Options;

{$R *.dfm}

{ TNumberingListDialogForm }

function TdxRichEditNumberingListDialogForm.GetActiveGallery: TdxNumberingListBoxHelper;
begin
  Result := TdxNumberingListBoxHelper(lcgTabControl.Items[lcgTabControl.ItemIndex].Data);
end;

function TdxRichEditNumberingListDialogForm.GetContinuePreviousList: Boolean;
begin
  Result := rbContinuePreviousList.Checked;
end;

function TdxRichEditNumberingListDialogForm.GetController: TdxNumberingListFormController;
begin
  Result := TdxNumberingListFormController(inherited Controller);
end;

function TdxRichEditNumberingListDialogForm.GetInitialTabPage(AList: TdxAbstractNumberingList): TdxLayoutGroup;
begin
  if AList = Controller.NoneList then
    Result := lcgBulleted
  else
    case GetNumberingListType(AList) of
      TdxNumberingType.MultiLevel:
        Result := lcgOutlineNumbered;
      TdxNumberingType.Simple:
        Result := lcgNumbered;
      else
        Result := lcgBulleted
    end;
end;

function TdxRichEditNumberingListDialogForm.GetNumberingListType(AList: TdxAbstractNumberingList): TdxNumberingType;
begin
  if not Assigned(AList) then
    Result := TdxNumberingType.Bullet
  else
    Result := TdxNumberingListHelper.GetListType(AList);
end;

function TdxRichEditNumberingListDialogForm.GetRestartNumbering: Boolean;
begin
  Result := rbRestartNumbering.Checked;
end;

procedure TdxRichEditNumberingListDialogForm.InitializeTabs;
  procedure InitializeTabsCore;
  var
    ANumberingOptions: TdxNumberingOptions;
  begin
    ANumberingOptions := Control.InnerControl.Options.DocumentCapabilities.Numbering;
    lcgBulleted.Enabled := ANumberingOptions.BulletedAllowed;
    lcgNumbered.Enabled := ANumberingOptions.SimpleAllowed;
    lcgOutlineNumbered.Enabled := ANumberingOptions.MultiLevelAllowed;
    FBulletedListBoxHelper := TdxNumberingListBoxHelper.Create(Control, Controller);
    FBulletedListBoxHelper.InitializeControl(lbBulleted, TdxNumberingType.Bullet, Controller.LevelIndex);
    lcgBulleted.Data := FBulletedListBoxHelper;
    FNumberingListBoxHelper := TdxNumberingListBoxHelper.Create(Control, Controller);
    FNumberingListBoxHelper.InitializeControl(lbNumbered, TdxNumberingType.Simple, Controller.LevelIndex);
    lcgNumbered.Data := FNumberingListBoxHelper;
    FOutlineNumberedListBoxHelper := TdxNumberingListBoxHelper.Create(Control, Controller);
    FOutlineNumberedListBoxHelper.InitializeControl(lbOutlineNumbered, TdxNumberingType.MultiLevel, Controller.LevelIndex);
    lcgOutlineNumbered.Data := FOutlineNumberedListBoxHelper;
  end;
var
  AAbstractList: TdxAbstractNumberingList;
  ASelectedTabPage: TdxLayoutGroup;
  AAllow: Boolean;
begin
  InitializeTabsCore;
  AAbstractList := Controller.GetSelectedAbstractNumberingList;
  ASelectedTabPage := GetInitialTabPage(AAbstractList);
  if not ASelectedTabPage.Enabled then
  begin
    ASelectedTabPage := ChangeSelectedTabPage(ASelectedTabPage);
    AAbstractList := nil;
  end;
  lcgTabControl.ItemIndex := ASelectedTabPage.Tag;
  lcgTabControlTabChanging(lcgTabControl, lcgTabControl.ItemIndex, AAllow);
  lcgTabControlTabChanged(lcgTabControl);
  lcgTabControl.OnTabChanging := lcgTabControlTabChanging;
  lcgTabControl.OnTabChanged := lcgTabControlTabChanged;
  ActiveGallery.SelectedAbstractList := AAbstractList;
end;

function TdxRichEditNumberingListDialogForm.IsBulletedTabSelected: Boolean;
begin
  Result := lcgTabControl.ItemIndex = lcgBulleted.Tag;
end;

function TdxRichEditNumberingListDialogForm.IsNumberedTabSelected: Boolean;
begin
  Result := lcgTabControl.ItemIndex = lcgNumbered.Tag;
end;

function TdxRichEditNumberingListDialogForm.IsOutlineNumberedTabSelected: Boolean;
begin
  Result := lcgTabControl.ItemIndex = lcgOutlineNumbered.Tag;
end;

procedure TdxRichEditNumberingListDialogForm.lcgTabControlTabChanged(Sender: TObject);
var
  AIndexOfParagraphInListBeforeCurrent: Integer;
  AIsParagraphValid: Boolean;
  AEnabled: Boolean;
begin
  AIndexOfParagraphInListBeforeCurrent := FNumberingListFormHelper.GetIndexOfParagraphInListBeforeCurrent();
  AIsParagraphValid := AIndexOfParagraphInListBeforeCurrent >= FNumberingListFormHelper.ParagraphIndexMinValue;
  AEnabled := (IsNumberedTabSelected or IsOutlineNumberedTabSelected) and
    not FNumberingListFormHelper.DoSelectedParagraphsHaveDifferentNumberingLists
    and AIsParagraphValid;
  rbRestartNumbering.Enabled := AEnabled;
  rbContinuePreviousList.Enabled := AEnabled;

  SubscribeGalleryEvents(TdxNumberingListBoxHelper(lcgTabControl.Items[lcgTabControl.ItemIndex].Data));
end;

procedure TdxRichEditNumberingListDialogForm.lcgTabControlTabChanging(Sender: TObject; ANewTabIndex: Integer;
  var Allow: Boolean);
var
  AGallery, APrevGallery: TdxNumberingListBoxHelper;
  AMaxIndex: Integer;
begin
  AGallery := TdxNumberingListBoxHelper(lcgTabControl.Items[ANewTabIndex].Data);
  APrevGallery := TdxNumberingListBoxHelper(lcgTabControl.Items[lcgTabControl.ItemIndex].Data);
  AMaxIndex := AGallery.ListBox.ControlItems.Count - 1;
  if APrevGallery.ListBox.SelectedIndex > AMaxIndex then
    AGallery.ListBox.SelectedIndex := AMaxIndex
  else
    AGallery.ListBox.SelectedIndex := APrevGallery.ListBox.SelectedIndex;
  UnsubscribeGalleryEvents(APrevGallery);
end;

procedure TdxRichEditNumberingListDialogForm.OnActiveGalleryMouseDoubleClick(Sender: TObject);
begin
  if TdxCustomContainerListBox(Sender).IsMouseClickInItems then
  begin
    ApplyChangesWithoutCustomization();
    ModalResult := mrOk;
    Close;
  end;
end;

procedure TdxRichEditNumberingListDialogForm.OnSelectedIndexChanged(Sender: TObject);
begin
  btnCustomize.Enabled := TdxCustomContainerListBox(Sender).SelectedIndex > 0;
end;

procedure TdxRichEditNumberingListDialogForm.SetContinuePreviousList(const Value: Boolean);
begin
  rbContinuePreviousList.Checked := Value;
end;

procedure TdxRichEditNumberingListDialogForm.SetRestartNumbering(const Value: Boolean);
begin
  rbRestartNumbering.Checked := Value;
end;

procedure TdxRichEditNumberingListDialogForm.ShowBulletedListForm(ASource: TdxAbstractNumberingList;
  ALevels: TdxListLevelCollection);
var
  AForm: TdxRichEditBulletedListDialogForm;
  AControllerParameters: TdxBulletedListFormControllerParameters;
begin
  AControllerParameters := TdxBulletedListFormControllerParameters.Create(Control, ALevels, Controller.LevelIndex);
  try
    AForm := TdxRichEditBulletedListDialogForm.Create(Self);
    try
      AForm.Initialize(AControllerParameters);
      if AForm.ShowModal = mrOk then
        FNumberingListFormHelper.ApplyChanges(ASource);
    finally
      AForm.Free;
    end;
  finally
    AControllerParameters.Free;
  end;
end;

procedure TdxRichEditNumberingListDialogForm.ShowMultiLevelNumberingListForm(ASource: TdxAbstractNumberingList;
  ALevels: TdxListLevelCollection);
begin
  FNumberingListFormHelper.ShowNumberingListFormCore(ASource, ALevels, True);
end;

procedure TdxRichEditNumberingListDialogForm.ShowSimpleNumberingListForm(ASource: TdxAbstractNumberingList;
  ALevels: TdxListLevelCollection);
begin
  FNumberingListFormHelper.ShowNumberingListFormCore(ASource, ALevels, False);
end;

procedure TdxRichEditNumberingListDialogForm.SubscribeGalleryEvents(AGallery: TdxNumberingListBoxHelper);
begin
  AGallery.ListBox.OnDblClick := OnActiveGalleryMouseDoubleClick;
  AGallery.ListBox.OnSelectedIndexChanged := OnSelectedIndexChanged;
end;

procedure TdxRichEditNumberingListDialogForm.UnSubscribeGalleryEvents(AGallery: TdxNumberingListBoxHelper);
begin
  AGallery.ListBox.OnDblClick := nil;
  AGallery.ListBox.OnSelectedIndexChanged := nil;
end;

procedure TdxRichEditNumberingListDialogForm.ApplyChangesWithoutCustomization;
begin
  FNumberingListFormHelper.ApplyChangesWithoutCustomization;
end;

procedure TdxRichEditNumberingListDialogForm.ApplyLocalization;
begin
  Caption := cxGetResourceString(@sdxRichEditNumberingListDialogForm);
  rbRestartNumbering.Caption := cxGetResourceString(@sdxRichEditNumberingListDialogRestartNumbering);
  rbContinuePreviousList.Caption := cxGetResourceString(@sdxRichEditNumberingListDialogContinuePreviousList);
  btnCustomize.Caption := cxGetResourceString(@sdxRichEditNumberingListDialogButtonCustomize);
  btnOk.Caption := cxGetResourceString(@sdxRichEditDialogButtonOk);
  btnCancel.Caption := cxGetResourceString(@sdxRichEditDialogButtonCancel);
  lcgBulleted.CaptionOptions.Text := cxGetResourceString(@sdxRichEditNumberingListDialogBulleted);
  lcgNumbered.CaptionOptions.Text := cxGetResourceString(@sdxRichEditNumberingListDialogNumbered);
  lcgOutlineNumbered.CaptionOptions.Text := cxGetResourceString(@sdxRichEditNumberingListDialogOutlineNumbered);
end;

function TdxRichEditNumberingListDialogForm.CreateController(
  AControllerParameters: TdxFormControllerParameters): TdxFormController;
begin
  Result := TdxNumberingListFormController.Create(AControllerParameters as TdxNumberingListFormControllerParameters);
end;

function TdxRichEditNumberingListDialogForm.CreateNumberingListFormCore(AListLevels: TdxListLevelCollection;
  AFirstParagraphStartIndex: Integer; AMultiLevels: Boolean): TdxRichEditCustomDialogForm;
var
  AParameters: TdxSimpleNumberingListFormControllerParameters;
begin
  if AMultiLevels then
    Result := TdxRichEditMultiLevelNumberingListDialogForm.Create(Self)
  else
    Result := TdxRichEditSimpleNumberingListDialogForm.Create(Self);
  AParameters := TdxSimpleNumberingListFormControllerParameters.Create(Control, AListLevels, Controller.LevelIndex, AFirstParagraphStartIndex);
  try
    Result.Initialize(AParameters);
  finally
    AParameters.Free;
  end;
end;

procedure TdxRichEditNumberingListDialogForm.InitializeForm;
begin
  FNumberingListFormHelper := TdxNumberingListFormHelper.Create(Self);
  InitializeTabs;
  InitializeNumberingMode;
end;

procedure TdxRichEditNumberingListDialogForm.InitializeNumberingMode;
var
  AIndexOfParagraphInListBeforeCurrent: Integer;
  AAreNumberingListsDifferent: Boolean;
begin
  AIndexOfParagraphInListBeforeCurrent := FNumberingListFormHelper.GetIndexOfParagraphInListBeforeCurrent();
  AAreNumberingListsDifferent := FNumberingListFormHelper.IsParagraphInDifferentListFromCurrent(AIndexOfParagraphInListBeforeCurrent);
  RestartNumbering := AAreNumberingListsDifferent;
  ContinuePreviousList := not AAreNumberingListsDifferent;
end;

procedure TdxRichEditNumberingListDialogForm.btnCustomizeClick(Sender: TObject);
var
  ATempDocumentModel: TdxDocumentModel;
  AClone: TdxAbstractNumberingList;
begin
  ATempDocumentModel := TdxDocumentModel.Create;
  try
    AClone := Controller.CreateAbstractNumberingListCopy(ATempDocumentModel, ActiveGallery.SelectedAbstractList);
    AClone.SetId(AClone.GenerateNewId);
    if IsBulletedTabSelected then
      ShowBulletedListForm(AClone, AClone.Levels)
    else
      if IsNumberedTabSelected then
        ShowSimpleNumberingListForm(AClone, AClone.Levels)
      else
        ShowMultiLevelNumberingListForm(AClone, AClone.Levels);
  finally
    ATempDocumentModel.Free;
  end;
end;

procedure TdxRichEditNumberingListDialogForm.btnOkClick(Sender: TObject);
begin
  if (ActiveGallery.ListBox.SelectedIndex < 0) then
    ModalResult := mrCancel
  else
    ApplyChangesWithoutCustomization;
end;

function TdxRichEditNumberingListDialogForm.ChangeSelectedTabPage(
  ACurrentSelectedTabPage: TdxLayoutGroup): TdxLayoutGroup;
begin
  if ACurrentSelectedTabPage = lcgBulleted then
  begin
    if lcgNumbered.Enabled then
      Result := lcgNumbered
    else
      Result := lcgOutlineNumbered
  end
  else
  if ACurrentSelectedTabPage = lcgNumbered then
  begin
    if lcgBulleted.Enabled then
      Result := lcgBulleted
    else
      Result := lcgOutlineNumbered
  end
  else
  if ACurrentSelectedTabPage = lcgOutlineNumbered then
  begin
    if lcgBulleted.Enabled then
      Result := lcgBulleted
    else
      Result := lcgNumbered
  end
  else
  begin
    Assert(false);
    Result := nil;
  end;
end;

destructor TdxRichEditNumberingListDialogForm.Destroy;
begin
  FBulletedListBoxHelper.Free;
  FNumberingListBoxHelper.Free;
  FOutlineNumberedListBoxHelper.Free;
  FNumberingListFormHelper.Free;
  inherited;
end;

end.
