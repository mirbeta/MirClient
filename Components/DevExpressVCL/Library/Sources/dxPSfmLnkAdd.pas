{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSfmLnkAdd;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Registry, ExtCtrls, StdCtrls, ComCtrls,
  Menus, ImgList, dxCore, dxPSForm, dxPSCompsProvider, dxPSCore, cxControls, cxContainer, cxEdit, cxLabel,
  cxCheckBox, IniFiles, cxLookAndFeelPainters, cxButtons, cxTextEdit, cxListView, cxPC, cxGroupBox,
  cxGraphics, cxLookAndFeels, dxLayoutcxEditAdapters, dxLayoutControlAdapters, dxLayoutContainer,
  dxLayoutLookAndFeels, cxClasses, dxLayoutControl, cxImageList, cxMaskEdit, cxDropDownEdit,
  Generics.Collections, Generics.Defaults;

type
  TSortOrder = (soNone, soUp, soDown);

  TdxAddReportLinkDlgDataOption = (adoShowDesignButton, adoShowDescriptionColumn, adoShowOnlyComponentsWOLinks,
    adoShowOnlyComponentsInActiveForm, adoAllowMultiSelect, adoShowNameEdit, adoShowCaptionEdit,
    adoShowHideCustomContainers, adoShowCreatorEdit, adoAllowSelectLinkClass);
  TdxAddReportLinkDlgDataOptions = set of TdxAddReportLinkDlgDataOption;

  TdxfmAddComponent = class(TCustomdxPSForm)
    btnCancel: TcxButton;
    btnDescription: TcxButton;
    btnDesign: TcxButton;
    btnHelp: TcxButton;
    btnOK: TcxButton;
    cbReportLinkClasses: TcxComboBox;
    chbxHideCustomContainers: TcxCheckBox;
    chbxOnlyInCurrentModule: TcxCheckBox;
    chbxOnlyUnLinked: TcxCheckBox;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutGroup4: TdxLayoutGroup;
    dxLayoutGroup11: TdxLayoutGroup;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem9: TdxLayoutItem;
    dxLayoutItem10: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    edCaption: TcxTextEdit;
    edCreator: TcxTextEdit;
    edName: TcxTextEdit;
    ilColumns: TcxImageList;
    ilItems: TcxImageList;
    lblCaption: TdxLayoutItem;
    lblCreator: TdxLayoutItem;
    lblName: TdxLayoutItem;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    libtnDesign: TdxLayoutItem;
    libtnHelp: TdxLayoutItem;
    lichbxHideCustomContainers: TdxLayoutItem;
    lichbxOnlyInCurrentModule: TdxLayoutItem;
    lichbxOnlyUnLinked: TdxLayoutItem;
    liReportLinkClasses: TdxLayoutItem;
    lvItems: TcxListView;
    miAdd: TMenuItem;
    miDesign: TMenuItem;
    miLine1: TMenuItem;
    pmItems: TPopupMenu;
    pnlCreatorHost: TdxLayoutGroup;
    pnlNoItems: TcxLabel;
    tbsItems: TdxLayoutGroup;

    procedure btnDescriptionClick(Sender: TObject);
    procedure btnDesignClick(Sender: TObject);
    procedure cbReportLinkClassesPropertiesChange(Sender: TObject);
    procedure cbReportLinkClassesPropertiesValidate(Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
    procedure chbxClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lvItemsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure lvItemsColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvItemsCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
    procedure lvItemsDblClick(Sender: TObject);
    procedure lvItemsResize(Sender: TObject);
    procedure lvItemsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure pmItemsPopup(Sender: TObject);
  private
    FComponentInfo: TDictionary<TObject, TObject>;
    FComponentPrinter: TdxComponentPrinter;
    FDesignBtnPressed: Boolean;
    FOptions: TdxAddReportLinkDlgDataOptions;
    FOriginalReportLinkName: string;
    FPrevListViewWndProc: TWndMethod;
    FReportLink: TBasedxReportLink;
    FReportLinkDescription: string;
    FSortedColumnIndex: Integer;
    FSortOrder: TSortOrder;

    function GetItemCount: Integer;
    function GetListViewHeader: HWND;
    function GetReportLinkCaption: string;
    function GetReportLinkCreator: string;
    function GetReportLinkName: string;
    function GetSelectedComponent: TComponent;
    function GetSelectedLinkClass: TdxReportLinkClass;
    procedure GetSelection(AList: TList; ALinkClasses: TList = nil);
    procedure SetReportLinkCaption(const Value: string);
    procedure SetReportLinkCreator(const Value: string);
    procedure SetReportLinkName(const Value: string);

    procedure AssignListViewHeaderImages;
    procedure ListViewWndProc(var Message: TMessage);
    procedure RestoreListViewWndProc;
    procedure SubstituteListViewWndProc;

    function CanApply: Boolean;
    function CanEditName: Boolean;
    function CanDesign: Boolean;
    function CheckUserInput: Boolean;
    procedure Initialize;
    procedure LoadStrings;
    function PreparedOptions: TdxPSGetComponentOptions;
    procedure PopulateLinkClasses;
    procedure RefreshColumns;
    procedure RefreshList;
    procedure RefreshSorting;
    procedure SetActiveControl;
    procedure SetColumnSortMark(AIndex: Integer; ASortOrder: TSortOrder);
    procedure SortColumn(Column: TListColumn; ASortOrder: TSortOrder);
    procedure UpdateControlsState;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
  protected
    procedure CreateWnd; override;
    procedure BeforeConstruction; override;

    procedure LoadComponentState(AIniFile: TCustomIniFile; const ASectionName: string);
    procedure LoadListViewSorting(AIniFile: TCustomIniFile; const ASectionName: string);
    procedure SaveComponentState(AIniFile: TCustomIniFile; const ASectionName: string);
    procedure SaveListViewSorting(AIniFile: TCustomIniFile; const ASectionName: string);

    property ListViewHeader: HWND read GetListViewHeader;
  public
    function Execute: Boolean;
    procedure LoadFromIniFile(AIniFile: TCustomIniFile; const ASectionName: string); override;
    procedure SaveToIniFile(AIniFile: TCustomIniFile; const ASectionName: string); override;

    property ItemCount: Integer read GetItemCount;
    property OriginalReportLinkName: string read FOriginalReportLinkName write FOriginalReportLinkName;
    property ReportLinkCaption: string read GetReportLinkCaption write SetReportLinkCaption;
    property ReportLinkCreator: string read GetReportLinkCreator write SetReportLinkCreator;
    property ReportLinkDescription: string read FReportLinkDescription write FReportLinkDescription;
    property ReportLinkName: string read GetReportLinkName write SetReportLinkName;
    property SelectedComponent: TComponent read GetSelectedComponent;
    property SelectedLinkClass: TdxReportLinkClass read GetSelectedLinkClass;
  end;

  { TdxAddReportLinkDlgData }

  PdxAddReportLinkDlgData = ^TdxAddReportLinkDlgData;
  TdxAddReportLinkDlgData = record
    Components: TList;
    ComponentsLinkClasses: TList;
    ComponentPrinter: TdxComponentPrinter;
    ReportLink: TBasedxReportLink;
    ReportLinkName: string;
    ReportLinkCaption: string;
    ReportLinkCreator: string;
    ReportLinkDescription: string;
    Options: TdxAddReportLinkDlgDataOptions;
    DesignBtnPressed: Boolean;
    Title: string;

    procedure Finalize;
    procedure Initialize;
  end;

function dxShowAddComponentsDlg(var AData: TdxAddReportLinkDlgData): Boolean;
implementation

{$R *.DFM}

uses
  CommCtrl, Math, dxPSRes, dxPSUtl, dxPSGlbl, dxPSPopupMan, dxPSfmEditDesc;

const
  sdxOnlyWithoutLinks = 'OnlyWithoutLinks';             // Don't Localize
  sdxOnlyInCurrentModule = 'OnlyInCurrentModule';       // Don't Localize
  sdxHideCustomContainers_ = 'HideCustomContainers';    // Don't Localize
  sdxSortedColumnIndex = 'SortedColumnIndex';           // Don't Localize
  sdxSortOrder = 'SortOrder';                           // Don't Localize

type

  TComponentInfo = class
  strict private
    FLinkClass: TdxReportLinkClass;
    FLinkClasses: TList;

    function GetCaption: string;
    function GetComponent: TComponent;
    function GetDescription: string;
    function GetLinkClass: TdxReportLinkClass;
    function GetLinkClasses: TList;
  protected
    FInfo: TdxComponentItem;
  public
    destructor Destroy; override;
    //
    property Caption: string read GetCaption;
    property Component: TComponent read GetComponent;
    property Description: string read GetDescription;
    property LinkClass: TdxReportLinkClass read GetLinkClass write FLinkClass;
    property LinkClasses: TList read GetLinkClasses;
  end;

function dxShowAddComponentsDlg(var AData: TdxAddReportLinkDlgData): Boolean;
var
  ADialog: TdxfmAddComponent;
begin
  Result := False;
  if AData.Components = nil then
    Exit;

  ADialog := TdxfmAddComponent.Create(nil);
  try
    ADialog.FOptions := AData.Options;
    ADialog.FComponentPrinter := AData.ComponentPrinter;
    ADialog.FReportLink := AData.ReportLink;
    ADialog.ReportLinkName := AData.ReportLinkName;
    ADialog.OriginalReportLinkName := ADialog.ReportLinkName;
    ADialog.ReportLinkCaption := AData.ReportLinkCaption;
    ADialog.ReportLinkCreator := AData.ReportLinkCreator;
    ADialog.ReportLinkDescription := AData.ReportLinkDescription;
    if AData.Title <> '' then
      ADialog.Caption := AData.Title;

    Result := ADialog.Execute;
    if Result then
    begin
      ADialog.GetSelection(AData.Components, AData.ComponentsLinkClasses);
      AData.DesignBtnPressed := ADialog.FDesignBtnPressed;
      if ADialog.SelectedComponent <> nil then
        AData.ReportLinkName := ADialog.ReportLinkName;
      AData.ReportLinkCaption := ADialog.ReportLinkCaption;
      AData.ReportLinkCreator := ADialog.ReportLinkCreator;
      AData.ReportLinkDescription := ADialog.ReportLinkDescription;
    end;
  finally
    ADialog.Free;
  end;
end;

function GetInfo(AItem: TListItem): TComponentInfo; inline;
begin
  if AItem <> nil then
    Result := TComponentInfo(AItem.Data)
  else
    Result := nil;
end;

{ TdxfmAddComponent }

procedure TdxfmAddComponent.FormCreate(Sender: TObject);
begin
  FSortedColumnIndex := -1;
  FComponentInfo := TObjectDictionary<TObject, TObject>.Create([doOwnsValues]);
  HelpContext := dxPSGlbl.dxhcAddLinkDlg;
  Application.MainForm.Caption := Caption;
  SubstituteListViewWndProc;
  AssignListViewHeaderImages;
  dxPSPopupMenuController.RegisterControl(lvItems);
end;

procedure TdxfmAddComponent.FormDestroy(Sender: TObject);
begin
  dxPSPopupMenuController.UnregisterControl(lvItems);
  RestoreListViewWndProc;
  FreeAndNil(FComponentInfo);
end;

procedure TdxfmAddComponent.cbReportLinkClassesPropertiesChange(Sender: TObject);
var
  AInfo: TComponentInfo;
begin
  AInfo := GetInfo(lvItems.Selected);
  if AInfo <> nil then
    AInfo.LinkClass := TdxReportLinkClass(cbReportLinkClasses.ItemObject);
  UpdateControlsState;
end;

procedure TdxfmAddComponent.cbReportLinkClassesPropertiesValidate(
  Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
begin
  Error := cbReportLinkClasses.ItemIndex < 0;
end;

procedure TdxfmAddComponent.chbxClick(Sender: TObject);
begin
  RefreshList;
  RefreshSorting;
end;

procedure TdxfmAddComponent.btnDesignClick(Sender: TObject);
begin
  FDesignBtnPressed := True;
  ModalResult := mrOk;
end;

procedure TdxfmAddComponent.lvItemsChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  if Change = ctState then
    UpdateControlsState;
end;

procedure TdxfmAddComponent.btnDescriptionClick(Sender: TObject);
var
  S: string;
begin
  S := ReportLinkDescription;
  if dxEditDescriptionDlg(S) then
    ReportLinkDescription := S;
end;

procedure TdxfmAddComponent.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if ModalResult = mrOK then
    CanClose := CheckUserInput;
end;

procedure TdxfmAddComponent.lvItemsColumnClick(Sender: TObject; Column: TListColumn);
const
  SortOrderToggleMap: array[TSortOrder] of TSortOrder = (soUp, soDown, soUp);
var
  KeyboardState: TKeyboardState;
begin
  Windows.GetKeyboardState(KeyboardState);
  if KeyboardState[VK_CONTROL] and $80 <> 0 then
  begin
    if FSortedColumnIndex <> -1 then
    begin
      RefreshList;
      SetColumnSortMark(FSortedColumnIndex, soNone);
      FSortOrder := soNone;
      FSortedColumnIndex := -1;
    end;
  end
  else
  begin
    FSortOrder := SortOrderToggleMap[FSortOrder];
    SortColumn(Column, FSortOrder);
  end;
end;

procedure TdxfmAddComponent.lvItemsCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  if Data = 0 then
    Compare := CompareText(Item1.Caption, Item2.Caption)
  else
    Compare := CompareText(Item1.SubItems[Data - 1], Item2.SubItems[Data - 1]);

  if FSortOrder = soUp then
    Compare := -Compare;
end;

procedure TdxfmAddComponent.lvItemsDblClick(Sender: TObject);
begin
  if btnOK.Enabled then
    ModalResult := mrOk;
end;

procedure TdxfmAddComponent.lvItemsResize(Sender: TObject);
const
  Offset = 8;
begin
  pnlNoItems.SetBounds(Offset, (lvItems.ClientHeight - pnlNoItems.Height) div 2,
    lvItems.ClientWidth - 2 * Offset, pnlNoItems.Height);
end;

procedure TdxfmAddComponent.lvItemsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  PopulateLinkClasses;
  UpdateControlsState;
end;

procedure TdxfmAddComponent.pmItemsPopup(Sender: TObject);
begin
  miAdd.Enabled := CanApply;
  miDesign.Enabled := CanDesign;
  miDesign.Visible := adoShowDesignButton in FOptions;
  miLine1.Visible := miDesign.Visible;
end;

function TdxfmAddComponent.Execute: Boolean;
begin
  Initialize;
  Result := ShowModal = mrOk;
end;

procedure TdxfmAddComponent.LoadFromIniFile(AIniFile: TCustomIniFile; const ASectionName: string);
begin
  inherited LoadFromIniFile(AIniFile, ASectionName);
  dxLoadListViewColumns(AIniFile, ASectionName, lvItems.InnerListView);
  LoadComponentState(AIniFile, ASectionName);
  LoadListViewSorting(AIniFile, dxValidatePath(ASectionName) + lvItems.Name);
end;

procedure TdxfmAddComponent.SaveToIniFile(AIniFile: TCustomIniFile; const ASectionName: string);
begin
  inherited SaveToIniFile(AIniFile, ASectionName);
  dxSaveListViewColumns(AIniFile, ASectionName, lvItems.InnerListView);
  SaveComponentState(AIniFile, ASectionName);
  SaveListViewSorting(AIniFile, dxValidatePath(ASectionName) + lvItems.Name);
end;

procedure TdxfmAddComponent.CreateWnd;
begin
  inherited CreateWnd;
  SendMessage(Handle, WM_SETICON, 1, Icon.Handle);
end;

procedure TdxfmAddComponent.BeforeConstruction;
begin
  inherited;
  Options := Options + [foSizeableDialog];
end;

procedure TdxfmAddComponent.LoadComponentState(AIniFile: TCustomIniFile; const ASectionName: string);
begin
  chbxOnlyUnLinked.Checked := AIniFile.ReadBool(ASectionName, sdxOnlyWithoutLinks, chbxOnlyUnLinked.Checked);
  chbxOnlyInCurrentModule.Checked := AIniFile.ReadBool(ASectionName, sdxOnlyInCurrentModule, chbxOnlyInCurrentModule.Checked);
  chbxHideCustomContainers.Checked := AIniFile.ReadBool(ASectionName, sdxHideCustomContainers_, chbxHideCustomContainers.Checked);
end;

procedure TdxfmAddComponent.LoadListViewSorting(AIniFile: TCustomIniFile; const ASectionName: string);
begin
  FSortedColumnIndex := AIniFile.ReadInteger(ASectionName, sdxSortedColumnIndex, FSortedColumnIndex);
  FSortOrder := TSortOrder(AIniFile.ReadInteger(ASectionName, sdxSortOrder, Integer(FSortOrder)));
end;

procedure TdxfmAddComponent.SaveListViewSorting(AIniFile: TCustomIniFile; const ASectionName: string);
begin
  AIniFile.WriteInteger(ASectionName, sdxSortedColumnIndex, FSortedColumnIndex);
  AIniFile.WriteInteger(ASectionName, sdxSortOrder, Integer(FSortOrder));
end;

procedure TdxfmAddComponent.SaveComponentState(AIniFile: TCustomIniFile; const ASectionName: string);
begin
  AIniFile.WriteBool(ASectionName, sdxOnlyWithoutLinks, chbxOnlyUnLinked.Checked);
  AIniFile.WriteBool(ASectionName, sdxOnlyInCurrentModule, chbxOnlyInCurrentModule.Checked);
  AIniFile.WriteBool(ASectionName, sdxHideCustomContainers_, chbxHideCustomContainers.Checked);
end;

function TdxfmAddComponent.GetItemCount: Integer;
begin
  Result := lvItems.Items.Count;
end;

function TdxfmAddComponent.GetListViewHeader: HWND;
begin
  lvItems.HandleNeeded;
  if lvItems.HandleAllocated then
    Result := ListView_GetHeader(lvItems.Handle)
  else
    Result := 0;
end;

function TdxfmAddComponent.GetReportLinkCaption: string;
begin
  Result := edCaption.Text;
end;

function TdxfmAddComponent.GetReportLinkCreator: string;
begin
  Result := edCreator.Text;
end;

function TdxfmAddComponent.GetReportLinkName: string;
begin
  Result := edName.Text;
end;

function TdxfmAddComponent.GetSelectedComponent: TComponent;
begin
  if lvItems.SelCount = 1 then
    Result := GetInfo(lvItems.Selected).Component
  else
    Result := nil;
end;

function TdxfmAddComponent.GetSelectedLinkClass: TdxReportLinkClass;
begin
  if lvItems.SelCount = 1 then
    Result := GetInfo(lvItems.Selected).LinkClass
  else
    Result := nil;
end;

procedure TdxfmAddComponent.GetSelection(AList: TList; ALinkClasses: TList = nil);
var
  AItem: TListItem;
  AItemInfo: TComponentInfo;
  ALinkClass: TdxReportLinkClass;
  I: Integer;
begin
  for I := 0 to lvItems.Items.Count - 1 do
  begin
    AItem := lvItems.Items[I];
    if AItem.Selected then
    begin
      AItemInfo := GetInfo(AItem);
      if ALinkClasses <> nil then
      begin
        ALinkClass := AItemInfo.LinkClass;
        if ALinkClass = nil then
          Continue;
        ALinkClasses.Add(ALinkClass);
      end;
      if AList <> nil then
        AList.Add(AItemInfo.Component);
    end;
  end;
end;

procedure TdxfmAddComponent.SetReportLinkCaption(const Value: string);
begin
  edCaption.Text := Value;
end;

procedure TdxfmAddComponent.SetReportLinkCreator(const Value: string);
begin
  edCreator.Text := Value;
end;

procedure TdxfmAddComponent.SetReportLinkName(const Value: string);
begin
  edName.Text := Value;
end;

procedure TdxfmAddComponent.AssignListViewHeaderImages;
var
  Header: HWND;
begin
  Header := ListViewHeader;
  if IsWindow(Header) and not IsWindow(Header_GetImageList(Header)) then
    Header_SetImageList(Header, ilColumns.Handle);
end;

procedure TdxfmAddComponent.ListViewWndProc(var Message: TMessage);
begin
  FPrevListViewWndProc(Message);
  if (FSortedColumnIndex <> -1) and (Message.Msg = WM_NOTIFY) then
  begin
    if (TWMNotify(Message).NMHdr^.hWndFrom = ListViewHeader) and (TWMNotify(Message).NMHdr^.Code = HDN_ENDTRACK) then
      SetColumnSortMark(FSortedColumnIndex, FSortOrder);
  end;
end;

procedure TdxfmAddComponent.RestoreListViewWndProc;
begin
  lvItems.WindowProc := FPrevListViewWndProc;
end;

procedure TdxfmAddComponent.SubstituteListViewWndProc;
begin
  FPrevListViewWndProc := lvItems.WindowProc;
  lvItems.WindowProc := ListViewWndProc;
end;

function TdxfmAddComponent.CanDesign: Boolean;
begin
  Result := dxPSDesignerClassByLinkClass(SelectedLinkClass) <> nil;
end;

function TdxfmAddComponent.CanApply: Boolean;
var
  AList: TList;
begin
  Result := False;
  if lvItems.SelCount > 0 then
  begin
    AList := TList.Create;
    try
      GetSelection(nil, AList);
      Result := lvItems.SelCount = AList.Count;
    finally
      AList.Free;
    end;
  end;
end;

function TdxfmAddComponent.CanEditName: Boolean;
begin
  Result := SelectedComponent <> nil;
end;

function TdxfmAddComponent.CheckUserInput: Boolean;
begin
  if adoShowNameEdit in FOptions then
  begin
    Result := IsValidIdent(ReportLinkName);
    if Result then
    begin
      Result := FComponentPrinter.Owner.FindComponent(ReportLinkName) = nil;
      if not Result then
        MessageError(Format(cxGetResourceString(@sdxComponentAlreadyExists), [ReportLinkName]));
    end
    else
      MessageError(Format(cxGetResourceString(@sdxInvalidComponentName), [ReportLinkName]));

    if not Result and edName.CanFocus then
    begin
      edName.Text := FOriginalReportLinkName;
      edName.SelectAll;
      ActiveControl := edName;
    end;
  end
  else
    Result := True;
end;

procedure TdxfmAddComponent.Initialize;
begin
  lblName.Visible := adoShowNameEdit in FOptions;
  lblCaption.Visible := adoShowCaptionEdit in FOptions;
  pnlCreatorHost.Visible := adoShowCreatorEdit in FOptions;
  libtnDesign.Visible := adoShowDesignButton in FOptions;

  CheckDialogFormHelpContext(Self, libtnHelp);

  lichbxOnlyInCurrentModule.Visible := adoShowOnlyComponentsInActiveForm in FOptions;
  lichbxOnlyUnLinked.Visible := adoShowOnlyComponentsWOLinks in FOptions;
  lichbxHideCustomContainers.Visible := adoShowHideCustomContainers in FOptions;

  lvItems.MultiSelect := adoAllowMultiSelect in FOptions;
  pnlNoItems.Parent := lvItems;

  RefreshColumns;
  RefreshList;
  LoadStrings;

  SetActiveControl;
  lvItemsResize(nil);
end;

procedure TdxfmAddComponent.LoadStrings;
begin
  if (FComponentPrinter = nil) or not (csDesigning in FComponentPrinter.ComponentState) then
    Caption := cxGetResourceString(@sdxAddReport);

  lblName.Caption := cxGetResourceString(@sdxName);
  lblCaption.Caption := cxGetResourceString(@sdxCaption);
  lblCreator.Caption := cxGetResourceString(@sdxCreator);
  btnDescription.Caption := cxGetResourceString(@sdxBtnDescription);

  tbsItems.Caption := cxGetResourceString(@sdxAvailableSources);

  chbxHideCustomContainers.Caption := cxGetResourceString(@sdxHideCustomContainers);
  chbxOnlyInCurrentModule.Caption := cxGetResourceString(@sdxOnlyComponentsInActiveForm);
  chbxOnlyUnLinked.Caption := cxGetResourceString(@sdxOnlyComponentsWithoutLinks);

  pnlNoItems.Caption := cxGetResourceString(@sdxThereAreNowItemsForShow);
  miAdd.Caption := cxGetResourceString(@sdxAddReport);
  miDesign.Caption := cxGetResourceString(@sdxAddAndDesignReport);

  btnOK.Caption := cxGetResourceString(@sdxBtnOK);
  btnDesign.Caption := cxGetResourceString(@sdxBtnDesign);
  btnCancel.Caption := cxGetResourceString(@sdxBtnCancel);
  btnHelp.Caption := cxGetResourceString(@sdxBtnHelp);
end;

function TdxfmAddComponent.PreparedOptions: TdxPSGetComponentOptions;
begin
  Result := [];
  if (adoShowOnlyComponentsWOLinks in FOptions) and chbxOnlyUnLinked.Checked then
    Include(Result, gcoExcludeExisting);
  if (adoShowOnlyComponentsInActiveForm in FOptions) and chbxOnlyInCurrentModule.Checked then
    Include(Result, gcoExcludeOutOfActiveForm);
  if (adoShowHideCustomContainers in FOptions) and chbxHideCustomContainers.Checked then
    Include(Result, gcoHideCustomContainers);
end;

procedure TdxfmAddComponent.PopulateLinkClasses;
var
  AItems: TStrings;
  AInfo: TComponentInfo;
  I: Integer;
begin
  AItems := cbReportLinkClasses.Properties.Items;
  AItems.BeginUpdate;
  try
    AItems.Clear;
    AInfo := GetInfo(lvItems.Selected);
    if AInfo <> nil then
    begin
      for I := 0 to AInfo.LinkClasses.Count - 1 do
        AItems.AddObject(TClass(AInfo.LinkClasses[I]).ClassName, TObject(AInfo.LinkClasses[I]));
      cbReportLinkClasses.ItemIndex := AItems.IndexOfObject(TObject(AInfo.LinkClass));
    end;
    if cbReportLinkClasses.ItemIndex < 0 then
      cbReportLinkClasses.ItemIndex := 0;
    liReportLinkClasses.Visible := (AItems.Count > 1) and (adoAllowSelectLinkClass in FOptions);
  finally
    AItems.EndUpdate;
  end;
  cbReportLinkClasses.ValidateEdit;
end;

procedure TdxfmAddComponent.RefreshColumns;
var
  ColumnWidths: array of Integer;
  I: Integer;
begin
  lvItems.Columns.BeginUpdate;
  try
    SetLength(ColumnWidths, lvItems.Columns.Count);
    for I := 0 to lvItems.Columns.Count - 1 do
      ColumnWidths[I] := lvItems.Columns[I].Width;

    lvItems.Columns.Clear;
    with lvItems.Columns.Add do
    begin
      Width := 2 * (lvItems.Width - GetSystemMetrics(SM_CXHSCROLL) - 6) div 3;
      Caption := cxGetResourceString(@sdxItemName);
    end;

    if adoShowDescriptionColumn in FOptions then
      with lvItems.Columns.Add do
      begin
        Width := (lvItems.Width - GetSystemMetrics(SM_CXHSCROLL) - 6) div 3;
        Caption := cxGetResourceString(@sdxItemDescription);
      end;

    for I := 0 to Min(lvItems.Columns.Count - 1, Length(ColumnWidths) - 1) do
      lvItems.Columns[I].Width := ColumnWidths[I];
  finally
    lvItems.ColumnClick := True;
    lvItems.Columns.EndUpdate;
  end;
end;

procedure TdxfmAddComponent.RefreshList;

  procedure PopulateList;
  var
    AComponentItem: TdxComponentItem;
    AItem: TListItem;
    AItemInfo: TComponentInfo;
    AStrings: TStrings;
    I: Integer;
  begin
    AStrings := TStringList.Create;
    try
      dxPSComponentProvidersFactory.GetComponents(FComponentPrinter, FReportLink, AStrings, PreparedOptions);
      for I := 0 to AStrings.Count - 1 do
      begin
        AComponentItem := AStrings.Objects[I] as TdxComponentItem;
        if not FComponentInfo.TryGetValue(AComponentItem.Component, TObject(AItemInfo)) then
        begin
          AItemInfo := TComponentInfo.Create;
          FComponentInfo.Add(AComponentItem.Component, AItemInfo);
        end;
        AItemInfo.FInfo := AComponentItem;

        AItem := lvItems.Items.Add;
        AItem.Caption := AStrings[I];
        AItem.Data := AItemInfo;
        AItem.SubItems.Add(AItemInfo.Description);
      end;
    finally
      AStrings.Free;
    end;
  end;

  procedure RestoreSelection(ASelection: TList);
  var
    I, J: Integer;
    AItem: TListItem;
  begin
    for I := 0 to ASelection.Count - 1 do
      for J := 0 to lvItems.Items.Count - 1 do
      begin
        AItem := TListItem(lvItems.Items[J]);
        if GetInfo(AItem).Component = ASelection[I] then
        begin
          AItem.Selected := True;
          AItem.MakeVisible(True);
          Break;
        end;
      end;
  end;

  procedure UpdateListViewState;
  const
    Styles: array[Boolean] of TViewStyle = (vsReport, vsList);
  begin
    lvItems.ViewStyle := Styles[ItemCount = 0];
    lvItems.ColumnClick := True;
    lvItems.Enabled := ItemCount <> 0;
    lvItems.HideSelection := ItemCount = 0;
    if (lvItems.SelCount = 0) and (ItemCount <> 0) then
      lvItems.Selected := lvItems.Items[0];
  end;

var
  ASavedSelection: TList;
begin
  lvItems.Items.BeginUpdate;
  try
    ASavedSelection := TList.Create;
    try
      GetSelection(ASavedSelection);
      lvItems.Items.Clear;
      PopulateList;
      RestoreSelection(ASavedSelection);
      UpdateListViewState;
    finally
      ASavedSelection.Free;
    end;
  finally
    lvItems.Items.EndUpdate;
  end;
  UpdateControlsState;
end;

procedure TdxfmAddComponent.RefreshSorting;
begin
  if (FSortedColumnIndex > -1) and (FSortedColumnIndex < lvItems.Columns.Count) and (FSortOrder <> soNone) then
    SortColumn(lvItems.Columns[FSortedColumnIndex], FSortOrder);
end;

procedure TdxfmAddComponent.SetActiveControl;
begin
  if lvItems.CanFocus then
    ActiveControl := lvItems
  else
    if edName.CanFocus then
    begin
      ActiveControl := edName;
      edName.SelectAll;
    end
    else
      if edCaption.CanFocus then
      begin
        ActiveControl := edCaption;
        edCaption.SelectAll;
      end
      else
        if edCreator.CanFocus then
         begin
           ActiveControl := edCreator;
           edCreator.SelectAll;
         end;
end;

procedure TdxfmAddComponent.SetColumnSortMark(AIndex: Integer; ASortOrder: TSortOrder);
const
  Format: Integer = HDF_IMAGE or HDF_BITMAP_ON_RIGHT;
  ImageIndexes: array[TSortOrder] of Integer = (-1, 0, 1);
var
  Header: HWND;
  HDItem: THDItem;
begin
  AssignListViewHeaderImages;
  Header := ListViewHeader;
  if IsWindow(Header) then
  begin
    FillChar(HDItem, SizeOf(HDItem), 0);
    HDItem.Mask := HDI_FORMAT or HDI_IMAGE;
    Header_GetItem(Header, AIndex, HDItem);
    if ASortOrder <> soNone then
      HDItem.fmt := HDItem.fmt or Format
    else
      HDItem.fmt := HDItem.fmt and not Format;
    HDItem.iImage := ImageIndexes[ASortOrder];
    Header_SetItem(Header, AIndex, HDItem);
  end;
end;

procedure TdxfmAddComponent.SortColumn(Column: TListColumn; ASortOrder: TSortOrder);
var
  AItem: TListItem;
begin
  dxPSCore.dxPSStartWait;
  try
    if (FSortedColumnIndex <> Column.Index) and (FSortedColumnIndex <> -1) then
      SetColumnSortMark(FSortedColumnIndex, soNone);
    FSortedColumnIndex := Column.Index;
    if ASortOrder <> soNone then
    begin
      lvItems.CustomSort(nil, MakeTTag(FSortedColumnIndex));
      SetColumnSortMark(FSortedColumnIndex, ASortOrder);
    end;
    AItem := lvItems.Selected;
    if AItem <> nil then
      AItem.MakeVisible(True);
  finally
    dxPSCore.dxPSStopWait;
  end;
  lvItems.Invalidate;
end;

procedure TdxfmAddComponent.UpdateControlsState;
begin
  edName.Enabled := CanEditName;
  edCaption.Enabled := ItemCount <> 0;
  edCreator.Enabled := ItemCount <> 0;

  btnDescription.Enabled := ItemCount <> 0;
  btnDesign.Enabled := CanDesign;
  btnOk.Enabled := CanApply;
  pnlNoItems.Visible := ItemCount = 0;
end;

procedure TdxfmAddComponent.CMDialogChar(var Message: TCMDialogChar);
begin
  inherited;
  if IsAccel(Message.CharCode, tbsItems.Caption) then
  begin
    Message.Result := 1;
    if lvItems.Enabled then
      ActiveControl := lvItems;
  end;
end;

{ TComponentInfo }

destructor TComponentInfo.Destroy;
begin
  FreeAndNil(FLinkClasses);
  FreeAndNil(FInfo);
  inherited;
end;

function TComponentInfo.GetCaption: string;
begin
  Result := FInfo.Caption;
end;

function TComponentInfo.GetComponent: TComponent;
begin
  Result := FInfo.Component;
end;

function TComponentInfo.GetDescription: string;
begin
  Result := FInfo.Description;
end;

function TComponentInfo.GetLinkClass: TdxReportLinkClass;
begin
  if FLinkClass <> nil then
    Result := FLinkClass
  else
    if LinkClasses.Count = 1 then
      Result := LinkClasses[0]
    else
      Result := nil;
end;

function TComponentInfo.GetLinkClasses: TList;
begin
  if FLinkClasses = nil then
    FLinkClasses := dxPSLinkClassByCompClassEx(Component.ClassType);
  Result := FLinkClasses;
end;

{ TdxAddReportLinkDlgData }

procedure TdxAddReportLinkDlgData.Finalize;
begin
  FreeAndNil(ComponentsLinkClasses);
  FreeAndNil(Components);
end;

procedure TdxAddReportLinkDlgData.Initialize;
begin
  FillChar(Self, SizeOf(Self), 0);
  ComponentsLinkClasses := TList.Create;
  Components := TList.Create;
end;

end.
