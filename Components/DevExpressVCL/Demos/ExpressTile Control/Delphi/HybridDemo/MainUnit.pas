unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, StdCtrls,
  Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, dxCustomTileControl, cxClasses,
  dxTileBar, HybridAppDM, HybridAppBaseFrame, dxTileControl, dxSkinsForm, dxSkinsDefaultPainters,
  dxSkinHybridApp, cxContainer, cxEdit, cxGroupBox, dxForms, dxCalloutPopup,
  ExtCtrls, cxTextEdit, cxMaskEdit, cxButtonEdit, dxCore;

type

  { TMainForm }

  TMainForm = class(TdxForm, IdxLocalizerListener)
    tbMain: TdxTileBar;
    tmMainMyWorldGroup: TdxTileControlGroup;
    tbiTasks: TdxTileBarItem;
    tbiEmployees: TdxTileBarItem;
    tbMainOperationsGroup: TdxTileControlGroup;
    tbiProducts: TdxTileBarItem;
    tbiCustomers: TdxTileBarItem;
    tbiSales: TdxTileBarItem;
    ptcProducts: TdxTileControl;
    ptcProductsFilterGroup: TdxTileControlGroup;
    ptiHDVideoPlayer: TdxTileControlItem;
    pti50inchPlasma: TdxTileControlItem;
    pti21inchMonitor: TdxTileControlItem;
    ptiRemoteControl: TdxTileControlItem;
    ptcCustomers: TdxTileControl;
    ptcCustomersFilterGroup: TdxTileControlGroup;
    ptiAllCustomers: TdxTileControlItem;
    ptiMyAccount: TdxTileControlItem;
    ptiJohnAccount: TdxTileControlItem;
    ptiTopStores: TdxTileControlItem;
    dxSkinController1: TdxSkinController;
    tbiTasksPrint: TdxTileBarItem;
    tbHiddenItemsGroup: TdxTileControlGroup;
    tbiEmployeeEdit: TdxTileBarItem;
    tbiCustomerEdit: TdxTileBarItem;
    tbiProductEdit: TdxTileBarItem;
    tbiSaleView: TdxTileBarItem;
    tbiSalesPrint: TdxTileBarItem;
    cxGroupBox1: TcxGroupBox;
    tbiLanguages: TdxTileBarItem;
    dxCalloutPopup1: TdxCalloutPopup;
    ptcLanguages: TdxTileControl;
    ptcLanguageLeftToRightGroup: TdxTileControlGroup;
    ptcLanguageEnglish: TdxTileControlItem;
    ptcLanguageArabic: TdxTileControlItem;
    ptcLanguageRightToLeftGroup: TdxTileControlGroup;
    procedure tbiEmployeeEditDeactivatingDetail(Sender: TdxCustomTileControl; AItem: TdxTileControlItem;
      var AAllow: Boolean);
    procedure tbiEmployeesActivateDetail(Sender: TdxTileControlItem);
    procedure tbiEmployeesClick(Sender: TdxTileControlItem);
    procedure ptiHDVideoPlayerClick(Sender: TdxTileControlItem);
    procedure ptiAllCustomersClick(Sender: TdxTileControlItem);
    procedure tbMainPopupActivate(Sender: TdxCustomTileControl; AItem: TdxTileControlItem);
    procedure FormCreate(Sender: TObject);
    procedure tbiLanguagesClick(Sender: TdxTileControlItem);
    procedure ptcLanguageEnglishClick(Sender: TdxTileControlItem);
    procedure ptcLanguageFarsiClick(Sender: TdxTileControlItem);
    procedure ptcLanguageArabicClick(Sender: TdxTileControlItem);
    procedure ptcLanguageHebrewClick(Sender: TdxTileControlItem);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FClickedItem: TdxTileControlItem;
    procedure ClearClickedItem;
    procedure UpdateLanguage;
    procedure Translate;
    procedure TranslationChanged;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  cxGeometry, dxCoreGraphics, LocalizationStrs;

type
  TdxTileBarItemAccess = class(TdxTileBarItem);

{ TMainForm }

procedure TMainForm.tbiEmployeesActivateDetail(Sender: TdxTileControlItem);
begin
  Screen.Cursor := crHourGlass;
  try
    if Sender.DetailOptions.DetailControl = nil then
      Sender.DetailOptions.DetailControl := GetDetailControlClass(Sender.Tag).Create(Self);
    SendMessage(Sender.DetailOptions.DetailControl.Handle, UM_BEFOREACTIVATE, 0, 0);
    PostMessage(Sender.DetailOptions.DetailControl.Handle, UM_AFTERACTIVATE, 0, 0);
    ClearClickedItem;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.tbiEmployeesClick(Sender: TdxTileControlItem);
begin
  FClickedItem := Sender;
end;

procedure TMainForm.tbiLanguagesClick(Sender: TdxTileControlItem);
begin
  dxCalloutPopup1.Popup(tbMain, TdxTileBarItemAccess(tbiLanguages).ViewInfo.Bounds);
end;

procedure TMainForm.tbMainPopupActivate(Sender: TdxCustomTileControl; AItem: TdxTileControlItem);
begin
  if (AItem = tbiCustomers) and (ptcCustomers.Controller.FocusedItem = nil) then
    ptcCustomers.Controller.FocusedItem := ptiAllCustomers;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  dxSkinController1.NativeStyle := False;

  tbMain.Controller.FocusedItem := tbiTasks;

  ptcLanguages.Controller.FocusedItem := ptcLanguageEnglish;
  dxCalloutPopup1.BorderColor := tbiLanguages.Style.GradientBeginColor;
  dxCalloutPopup1.Color := dxCalloutPopup1.BorderColor;
  dxResourceStringsRepository.AddListener(Self);
  Translate;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  dxResourceStringsRepository.RemoveListener(Self);
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  ADesktopWorkArea: TRect;
begin
  ADesktopWorkArea := GetDesktopWorkArea(cxPoint(Left, Top));
  if (Width > cxRectWidth(ADesktopWorkArea)) or (Height > cxRectHeight(ADesktopWorkArea)) then
    WindowState := wsMaximized;
end;

procedure TMainForm.ptcLanguageArabicClick(Sender: TdxTileControlItem);
begin
  UpdateLanguage;
end;

procedure TMainForm.ptcLanguageEnglishClick(Sender: TdxTileControlItem);
begin
  UpdateLanguage;
end;

procedure TMainForm.ptcLanguageFarsiClick(Sender: TdxTileControlItem);
begin
  UpdateLanguage;
end;

procedure TMainForm.ptcLanguageHebrewClick(Sender: TdxTileControlItem);
begin
  UpdateLanguage;
end;

procedure TMainForm.ptiAllCustomersClick(Sender: TdxTileControlItem);
begin
  DM.ApplyCustomersFilter(Sender.Tag - 1);
  tbMain.HidePopupWindow;
end;

procedure TMainForm.ptiHDVideoPlayerClick(Sender: TdxTileControlItem);
begin
  DM.ApplyProductFilter(1000 + Sender.Tag);
  tbMain.HidePopupWindow;
end;

procedure TMainForm.tbiEmployeeEditDeactivatingDetail(Sender: TdxCustomTileControl; AItem: TdxTileControlItem;
  var AAllow: Boolean);
var
  ADetailControl: TWinControl;
begin
  ADetailControl := AItem.DetailOptions.DetailControl;
  AAllow := (ADetailControl = nil) or
    ((ADetailControl is TfrmBase) and (FClickedItem <> TfrmBase(ADetailControl).ParentFrameTileItem));
  if AAllow and (ADetailControl <> nil) and (ADetailControl is TfrmBase) then
    AAllow := TfrmBase(ADetailControl).CanDeactivate;
  if not AAllow then
    tbMain.Controller.FocusedItem := TfrmBase(ADetailControl).ParentFrameTileItem;
  ClearClickedItem;
end;

procedure TMainForm.ClearClickedItem;
begin
  FClickedItem := nil;
end;

procedure TMainForm.UpdateLanguage;
const
  Map: array[Boolean] of TBiDiMode = (bdRightToLeft, bdLeftToRight);
begin
  Application.BiDiMode := Map[ptcLanguages.Controller.FocusedItem = ptcLanguageEnglish];

  DM.cxLocalizer1.Locale := ptcLanguages.Controller.FocusedItem.Tag;
end;

procedure TMainForm.Translate;
begin
  tmMainMyWorldGroup.Caption.Text := cxGetResourceString(@sMainMenuMyWorldCaption);
  tbMainOperationsGroup.Caption.Text := cxGetResourceString(@sMainMenuOperationsCaption);
  tbiTasks.Text3.Value := cxGetResourceString(@sMainMenuTasksCaption);
  tbiEmployees.Text3.Value := cxGetResourceString(@sMainMenuEmployeesCaption);
  tbiLanguages.Text3.Value := cxGetResourceString(@sMainMenuLanguageCaption);
  tbiProducts.Text3.Value := cxGetResourceString(@sMainMenuProductsCaption);
  tbiCustomers.Text3.Value := cxGetResourceString(@sMainMenuCustomersCaption);
  tbiSales.Text3.Value := cxGetResourceString(@sMainMenuSalesCaption);

  ptcProductsFilterGroup.Caption.Text := cxGetResourceString(@sSubMenuCustomFilterCaption);
  ptiHDVideoPlayer.Text1.Value := cxGetResourceString(@sProductsSubMenuHDVideoPlayerCaption);
  pti50inchPlasma.Text1.Value := cxGetResourceString(@sProductsSubMenu50inchPlasmaCaption);
  pti21inchMonitor.Text1.Value := cxGetResourceString(@sProductsSubMenu21inchMonitorCaption);
  ptiRemoteControl.Text1.Value := cxGetResourceString(@sProductsSubMenuRemoteControlCaption);

  ptcCustomersFilterGroup.Caption.Text := cxGetResourceString(@sSubMenuCustomFilterCaption);
  ptiAllCustomers.Text1.Value := cxGetResourceString(@sCustomersSubMenuAllCustomersCaption);
  ptiMyAccount.Text1.Value := cxGetResourceString(@sCustomersSubMenuMyAccountCaption);
  ptiJohnAccount.Text1.Value := cxGetResourceString(@sCustomersSubMenuJohnsAccountCaption);
  ptiTopStores.Text1.Value := cxGetResourceString(@sCustomersSubMenuTopStoresCaption);

  ptcLanguageLeftToRightGroup.Caption.Text := cxGetResourceString(@sLanguageSubMenuLTRCaption);
  ptcLanguageRightToLeftGroup.Caption.Text := cxGetResourceString(@sLanguageSubMenuRTLCaption);
  ptcLanguageEnglish.Text1.Value := cxGetResourceString(@sLanguageSubMenuEnglishCaption);
  ptcLanguageArabic.Text1.Value := cxGetResourceString(@sLanguageSubMenuArabicCaption);
  if DM.cxLocalizer1.Locale <> 0 then
  begin
    ptcLanguageEnglish.Text1.Value := ptcLanguageEnglish.Text1.Value + ' (English)';
    ptcLanguageArabic.Text1.Value := ptcLanguageArabic.Text1.Value + ' (Arabic)';
  end;
end;

procedure TMainForm.TranslationChanged;
begin
  Translate;
end;

initialization
  UseLatestCommonDialogs := False;

end.
