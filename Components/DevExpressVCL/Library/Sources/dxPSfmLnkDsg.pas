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

unit dxPSfmLnkDsg;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus, StdCtrls, ExtCtrls, ComCtrls,
  Buttons, ImgList, DesignIntf, DesignWindows, dxCore, cxGraphics, cxControls, cxContainer, cxLookAndFeels,
  cxLookAndFeelPainters, cxButtons, cxEdit, cxGroupBox, cxListBox, dxPSCore, dxPSfmLnkAdd, dxPSDsgProxies, dxPSGlbl,
  dxLayoutContainer, dxLayoutControlAdapters, dxLayoutLookAndFeels, cxClasses, dxLayoutControl, cxImageList, cxCustomListBox;

type
  TdxfmReportLinkDesignWindow = class(TDesignWindow)
    pmLinks: TPopupMenu;
    miAdd: TMenuItem;
    miDelete: TMenuItem;
    miLine1: TMenuItem;
    miShowDesigner: TMenuItem;
    miLine2: TMenuItem;
    miSelectAll: TMenuItem;
    miChangeComponent: TMenuItem;
    miPrintPreview: TMenuItem;
    miPrint: TMenuItem;
    miPageSetup: TMenuItem;
    miLine3: TMenuItem;
    miCopy: TMenuItem;
    miCut: TMenuItem;
    miPaste: TMenuItem;
    miRestoreDefaults: TMenuItem;
    miLine: TMenuItem;
    miShowButtons: TMenuItem;
    miLine5: TMenuItem;
    miMoveUp: TMenuItem;
    miMoveDown: TMenuItem;
    miBackgroundEffects: TMenuItem;
    miAddStandard: TMenuItem;
    N1: TMenuItem;
    miBackgroundClear: TMenuItem;
    N2: TMenuItem;
    miRestoreOriginal: TMenuItem;
    miSetAsCurrent: TMenuItem;
    miEdit: TMenuItem;
    N3: TMenuItem;
    miBackground: TMenuItem;
    miAddExisting: TMenuItem;
    pmAdd: TPopupMenu;
    miAdd1: TMenuItem;
    miAddStandard1: TMenuItem;
    miAddExisting1: TMenuItem;
    miLine6: TMenuItem;
    miAddComposition1: TMenuItem;
    miAddComposition: TMenuItem;
    miLine7: TMenuItem;
    lbxLinks: TcxListBox;
    btnAdd: TcxButton;
    btnDelete: TcxButton;
    btnShowDesigner: TcxButton;
    btnSelectAll: TcxButton;
    btnRestoreOriginal: TcxButton;
    btnChangeComponent: TcxButton;
    btnPrintPreview: TcxButton;
    btnPrint: TcxButton;
    btnPageSetup: TcxButton;
    btnMoveUp: TcxButton;
    btnMoveDown: TcxButton;
    btnRestoreDefaults: TcxButton;
    ilLinks: TcxImageList;
    lcMainGroup_Root: TdxLayoutGroup;
    lcMain: TdxLayoutControl;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutItem1: TdxLayoutItem;
    pnlButtons: TdxLayoutGroup;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutItem9: TdxLayoutItem;
    dxLayoutItem10: TdxLayoutItem;
    dxLayoutItem11: TdxLayoutItem;
    dxLayoutItem12: TdxLayoutItem;
    dxLayoutItem13: TdxLayoutItem;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure AddClick(Sender: TObject);
    procedure AddStandardClick(Sender: TObject);
    procedure AddExistingClick(Sender: TObject);
    procedure lbxLinksClick(Sender: TObject);
    procedure LinkDesignClick(Sender: TObject);
    procedure SetAsCurrentClick(Sender: TObject);
    procedure LinkChangeComponentClick(Sender: TObject);
    procedure RestoreDefaultsClick(Sender: TObject);
    procedure RestoreOriginalClick(Sender: TObject);
    procedure PageSetupClick(Sender: TObject);
    procedure PrintPreviewClick(Sender: TObject);
    procedure PrintClick(Sender: TObject);
    procedure lbxLinksDblClick(Sender: TObject);
    procedure lbxLinksStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure lbxLinksEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure lbxLinksDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lbxLinksDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbxLinksKeyPress(Sender: TObject; var Key: Char);
    procedure EditClick(Sender: TObject);
    procedure MoveUpClick(Sender: TObject);
    procedure MoveDownClick(Sender: TObject);
    procedure BackgroundClick(Sender: TObject);
    procedure ClearBackgroundClick(Sender: TObject);
    procedure pmLinksPopup(Sender: TObject);
    procedure ShowButtonsClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure AddCompositionClick(Sender: TObject);
    procedure lbxLinksDrawItem(AControl: TcxListBox; ACanvas: TcxCanvas;
      AIndex: Integer; ARect: TRect; AState: TOwnerDrawState);
    procedure lbxLinksMeasureItem(AControl: TcxListBox; AIndex: Integer;
      var Height: Integer);
  private
    FController: TCustomdxComponentPrinter;
    FSaveCursor: TCursor;
    FSaveDragIndex: Integer;

    function GetControllerDesigner: TAbstractdxReportLinkDesigner;
    function GetCurrentLink: TBasedxReportLink;
    function GetLink(Index: Integer): TBasedxReportLink;
    function GetLinkCount: Integer;
    function GetLinkDescription(Index: Integer): string;
    function GetRegistryPath: string;
    function GetSelected(Index: Integer): Boolean;
    function GetSelectedCount: Integer;
    procedure SetController(Value: TCustomdxComponentPrinter);
    procedure SetSelected(Index: Integer; Value: Boolean);

    function CalculateLinkCaptionWidth: Integer;
    function CalculateLinkDescriptionOffset: Integer;
    function CalculateLinkDescriptionWidth: Integer;
    function CalculateLinkMaxRowWidth: Integer;

    function CanAdd: Boolean;
    function CanAddExisting: Boolean;
    function CanBackgroundClear: Boolean;
    function CanBackgroundEffects: Boolean;
    function CanChangeComponent: Boolean;
    function CanCopy: Boolean;
    function CanCut: Boolean;
    function CanDelete: Boolean;
    function CanMoveDown: Boolean;
    function CanMoveUp: Boolean;
    function CanPaste: Boolean;
    function CanPageSetup: Boolean;
    function CanPrint: Boolean;
    function CanPrintPreview: Boolean;
    function CanRestoreDefaults: Boolean;
    function CanRestoreOriginal: Boolean;
    function CanSelectAll: Boolean;
    function CanSetAsCurrent: Boolean;
    function CanShowDesigner: Boolean;
    procedure CheckAddLink;
    procedure CheckDeleteLink;
    procedure Copy;
    procedure Cut;
    procedure Delete;
    procedure DeleteItem(AItem: TBasedxReportLink);
    procedure DrawDragRect;
    function GetMinWindowSize: TPoint;
    procedure GetSelections(const ASelections: TdxDesignSelectionList);
    function IndexOf(AItem: TBasedxReportLink): Integer;
    procedure InternalAddLink(ALinkClass: TdxReportLinkClass; AComponent: TComponent;
      const AName, ACaption, ACreator, ADescription: string; AShowDesigner: Boolean);
    procedure MakeLinkable(AComponent: TComponent);
    procedure MoveSelection(ADelta: Integer);
    procedure Paste;
    procedure PrepareAddExistingItem(AMenuItem: TMenuItem);
    procedure RefreshList;
    procedure RestoreLayout;
    procedure SaveLayout;
    procedure Select(AItem: TPersistent; AddToSelection: Boolean);
    procedure SelectAll;
    procedure SelectController;
    procedure StartWait;
    procedure StopWait;
    procedure UpdateCaption;
    procedure UpdateControlsState;
    procedure UpdateHScrollBar;
    procedure UpdateItem(AItem: TBasedxReportLink);
    procedure UpdateMenuState;
    procedure UpdateSelections(const ASelections: TdxDesignSelectionList);
    procedure WMAppCommand(var Message: TMessage); message WM_APPCOMMAND;
    procedure WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
    procedure WMNCCreate(var Message: TWMNCCreate); message WM_NCCREATE;
    procedure WMNCDestroy(var Message: TWMNCCreate); message WM_NCDESTROY;
  protected
    procedure Activated; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function UniqueName(Comp: TComponent): string; override;
    property LinkDescriptions[Index: Integer]: string read GetLinkDescription;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function EditAction(Action: TEditAction): Boolean; override;
    procedure ItemDeleted(const ADesigner: IDesigner; Item: TPersistent); override;
    procedure ItemsModified(const Designer: IDesigner); override;
    procedure SelectionChanged(const ADesigner: IDesigner; const ASelection: IDesignerSelections); override;
    function GetEditState: TEditState; override;

    property Controller: TCustomdxComponentPrinter read FController write SetController;
    property ControllerDesigner: TAbstractdxReportLinkDesigner read GetControllerDesigner;
    property CurrentLink: TBasedxReportLink read GetCurrentLink;
    property LinkCount: Integer read GetLinkCount;
    property Links[Index: Integer]: TBasedxReportLink read GetLink;
    property RegistryPath: string read GetRegistryPath;
    property Selected[Index: Integer]: Boolean read GetSelected write SetSelected;
    property SelectedCount: Integer read GetSelectedCount;
  end;

procedure dxShowReportLinkDesigner(AComponentPrinter: TCustomdxComponentPrinter;
  AFormDesigner: TFormDesigner);
function dxReportLinkUniqueName(AComponentPrinter: TCustomdxComponentPrinter;
  AComponent: TComponent): string;

implementation

{$R *.DFM}

uses
  DesignConst, ComponentDesigner, Registry, TypInfo, CommCtrl, ColnEdit, dxPSRes, dxPrnPg, dxBkgnd, dxPSPrVwDsg,
  dxPSPopupMan, dxPSfmLnkAddE, dxPSUtl, dxPSContainerLnk, dxPSCompsProvider, dxPSEngn, dxPSImgs, dxCoreGraphics;

const
  sdxButtonBar = 'ButtonBar';                  // Don't localize
  sdxWidth = 'Width';                          // Don't localize
  sdxHeight = 'Height';                        // Don't localize

  sdxCantPasteComponent = 'Can''t paste component %s here !';
  sdxCantCreateUniqueName = 'Can''t create unique name for %s.';
  sdxLinkDesigner = 'Editing %s%s%s Links';
  sdxChangeComponent = 'Change Component';

  sdxLinked = 'Linked';
  sdxLinkedTo = 'Linked to';
  sdxUnassigned = 'Unassigned';

type
  TdxPSDesignTimedxComponentsProvider = class(TAbstractdxPSComponentsProvider)
  private
    FComponents: TStrings;
    FExistingComponents: TList;
    FOptions: TdxPSGetComponentOptions;
    FReportLink: TBasedxReportLink;
    procedure AppendComponent(AComponent: TComponent; const APathName: string);
    procedure EnumComponentsProc(const S: string);
    procedure EnumReportLinksProc(const S: string);
    procedure GetExistingComponents;
    function IsComponentAccessible(AComponent: TComponent): Boolean;
  public
    function ActiveDesigner: TFormDesigner;
    procedure GetComponents(AComponentPrinter: TdxComponentPrinter; AReportLink: TBasedxReportLink;
      AComponents: TStrings; AnOptions: TdxPSGetComponentOptions); override;
  end;

  TdxReportLinkDesigner = class(TAbstractdxReportLinkDesigner)
  private
    FDesignWindow: TdxfmReportLinkDesignWindow;
    FFormDesigner: TFormDesigner;
    FUpdateCount: Integer;
    function GetDesignWindow: TdxfmReportLinkDesignWindow;
    procedure Activate;
  protected
    procedure Modified; override;
    procedure Update(AItem: TBasedxReportLink); override;
  public
    constructor Create(AComponentPrinter: TCustomdxComponentPrinter; AFormDesigner: TFormDesigner);
    destructor Destroy; override;

    procedure BeginUpdate; override;
    procedure CancelUpdate; override;
    procedure EndUpdate; override;

    property DesignWindow: TdxfmReportLinkDesignWindow read GetDesignWindow;
    property FormDesigner: TFormDesigner read FFormDesigner;
  end;

procedure dxShowReportLinkDesigner(AComponentPrinter: TCustomdxComponentPrinter; AFormDesigner: TFormDesigner);
begin
  if AComponentPrinter.ReportLinkDesigner = nil then
    TdxReportLinkDesigner.Create(AComponentPrinter, AFormDesigner);
  TdxReportLinkDesigner(AComponentPrinter.ReportLinkDesigner).Activate;
end;

function dxReportLinkUniqueName(AComponentPrinter: TCustomdxComponentPrinter;
  AComponent: TComponent): string;
var
  I, J: Integer;
  S: string;
  NameExists: Boolean;
  Item: TBasedxReportLink;
begin
  S := AComponentPrinter.Name + AComponentPrinter.GetNewLinkName(TBasedxReportLink(AComponent));
  for I := 1 to High(Integer) do
  begin
    Result := Format(S, [I]);
    NameExists := False;
    Item := AComponentPrinter.LinkByName(Result);
    if Item = nil then
    begin
      for J := 0 to AComponentPrinter.Owner.ComponentCount - 1 do
        if CompareText(AComponentPrinter.Owner.Components[J].Name, Result) = 0 then
        begin
          NameExists := True;
          Break;
        end;
      if not NameExists then Exit;
    end;
  end;
  raise EdxException.CreateFmt(sdxCantCreateUniqueName, [AComponent.ClassName]);
end;

{ TdxPSDesignTimedxComponentsProvider }

function TdxPSDesignTimedxComponentsProvider.ActiveDesigner: TFormDesigner;
begin
  Result := ComponentDesigner.ActiveRoot.GetDesigner;
end;

procedure TdxPSDesignTimedxComponentsProvider.GetComponents(AComponentPrinter: TdxComponentPrinter;
  AReportLink: TBasedxReportLink; AComponents: TStrings; AnOptions: TdxPSGetComponentOptions);
var
  Root: TComponent;
begin
  if ActiveDesigner = nil then Exit;

  FComponents := AComponents;
  FOptions := AnOptions;
  FReportLink := AReportLink;

  FExistingComponents := nil;
  if gcoExcludeExisting in FOptions then
    FExistingComponents := TList.Create;
  try
    if FExistingComponents <> nil then
      GetExistingComponents;
    Root := ActiveDesigner.GetRoot;
    if IsComponentAccessible(Root) then
      AppendComponent(Root, Root.Name);
    ActiveDesigner.GetComponentNames(
      GetTypeData(PTypeInfo(TComponent.ClassInfo)), EnumComponentsProc);
  finally
    FreeAndNil(FExistingComponents);
  end;
end;

procedure TdxPSDesignTimedxComponentsProvider.AppendComponent(AComponent: TComponent;
  const APathName: string);
var
  ComponentItem: TdxComponentItem;
begin
  if (not (gcoExcludeOutOfActiveForm in FOptions) or (Pos('.', APathName) = 0)) and
    (((FReportLink = nil) and dxPSIsSupportedCompClass(AComponent)) or
     ((FReportLink <> nil) and FReportLink.Supports(AComponent) and
    (FReportLink.Component <> AComponent))) then
  begin
    ComponentItem := TdxComponentItem.Create(AComponent, APathName, AComponent.ClassName);
    FComponents.AddObject(ComponentItem.Caption, ComponentItem);
  end;
end;

procedure TdxPSDesignTimedxComponentsProvider.EnumComponentsProc(const S: string);
var
  Component: TComponent;
begin
  if ActiveDesigner <> nil then
  begin
    Component := ActiveDesigner.GetComponent(S);
    if IsComponentAccessible(Component) then
      AppendComponent(Component, S);
  end;
end;

procedure TdxPSDesignTimedxComponentsProvider.EnumReportLinksProc(const S: string);
var
  Component: TComponent;
begin
  if ActiveDesigner <> nil then
  begin
    Component := ActiveDesigner.GetComponent(S);
    if (Component is TBasedxReportLink) and (TBasedxReportLink(Component).Component <> nil) then
      FExistingComponents.Add(TBasedxReportLink(Component).Component);
  end;
end;

procedure TdxPSDesignTimedxComponentsProvider.GetExistingComponents;
begin
  if ActiveDesigner <> nil then
    ActiveDesigner.GetComponentNames(GetTypeData(TComponent.ClassInfo), EnumReportLinksProc);
end;

function TdxPSDesignTimedxComponentsProvider.IsComponentAccessible(AComponent: TComponent): Boolean;
var
  Inaccessible: Boolean;
begin
  Inaccessible := ((AComponent = nil) or (AComponent is TBasedxReportLink) or (AComponent is TCustomdxComponentPrinter)) or
    ((FExistingComponents <> nil) and (FExistingComponents.IndexOf(AComponent) <> -1)) or
    ((gcoHideCustomContainers in FOptions) and dxPSContainerLnk.dxPSIsComponentContainer(AComponent));
  Result := not Inaccessible;
end;

{ TdxReportLinkDesigner }

constructor TdxReportLinkDesigner.Create(AComponentPrinter: TCustomdxComponentPrinter;
  AFormDesigner: TFormDesigner);
begin
  inherited Create(AComponentPrinter);
  FFormDesigner := AFormDesigner;
end;

destructor TdxReportLinkDesigner.Destroy;
begin
  if FDesignWindow <> nil then
  begin
    FDesignWindow.Designer := nil;
    FDesignWindow.Free;
  end;
  inherited Destroy;
end;

procedure TdxReportLinkDesigner.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TdxReportLinkDesigner.CancelUpdate;
begin
  if FUpdateCount <> 0 then
    Dec(FUpdateCount);
end;

procedure TdxReportLinkDesigner.EndUpdate;
begin
  if FUpdateCount <> 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
      Update(nil);
  end;
end;

function TdxReportLinkDesigner.GetDesignWindow: TdxfmReportLinkDesignWindow;
begin
  if FDesignWindow = nil then
  begin
    FDesignWindow := TdxfmReportLinkDesignWindow.Create(nil);
    FDesignWindow.Designer := FormDesigner;
    FDesignWindow.Controller := ComponentPrinter;
  end;
  Result := FDesignWindow;
end;

procedure TdxReportLinkDesigner.Activate;
begin
  DesignWindow.Show;
end;

procedure TdxReportLinkDesigner.Modified;
begin
  if FormDesigner <> nil then
    FormDesigner.Modified;
end;

procedure TdxReportLinkDesigner.Update(AItem: TBasedxReportLink);
begin
  if (FUpdateCount = 0) and (FDesignWindow <> nil) then
    DesignWindow.UpdateItem(AItem);
end;

{ TdxfmReportLinkDesigner }

constructor TdxfmReportLinkDesignWindow.Create(AOwner: TComponent);

  function MakeTTag(AnAction: TEditAction): Integer;
  begin
    Result := Integer(AnAction);
  end;

begin
  HelpContext := dxPSGlbl.dxhcReportLinkDesignWindow;
  inherited Create(AOwner);
  dxLoadImageListFromResources(ilLinks, IDIL_DXPSDESIGNWINDOWMENU);

  miCut.Tag := MakeTTag(eaCut);
  miCopy.Tag := MakeTTag(eaCopy);
  miPaste.Tag := MakeTTag(eaPaste);
  miDelete.Tag := MakeTTag(eaDelete);
  miSelectAll.Tag := MakeTTag(eaSelectAll);

  btnDelete.Tag := MakeTTag(eaDelete);
  btnSelectAll.Tag := MakeTTag(eaSelectAll);
  Constraints.MinHeight := Height;

  RestoreLayout;
  SetControlLookAndFeel(Self, dxPSEngine.DialogsLookAndFeel);
end;

destructor TdxfmReportLinkDesignWindow.Destroy;
begin
  SaveLayout;
  if ControllerDesigner <> nil then
    TdxReportLinkDesigner(ControllerDesigner).FDesignWindow := nil;
  inherited Destroy;
end;

procedure TdxfmReportLinkDesignWindow.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  MenuItem: TMenuItem;
begin
  MenuItem := pmLinks.FindItem(Key, fkShortCut);
  if (MenuItem <> nil) and MenuItem.Enabled and MenuItem.Visible then
  begin
    MenuItem.Click;
    Key := 0;
  end
  else
    if Key = VK_ESCAPE then
    begin
      Close;
      Key := 0;
    end;
end;

procedure TdxfmReportLinkDesignWindow.FormResize(Sender: TObject);
begin
  UpdateHScrollBar;
end;

procedure TdxfmReportLinkDesignWindow.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if not (csDestroying in Controller.ComponentState) then
    SelectController;
  Action := caFree;
end;

procedure TdxfmReportLinkDesignWindow.AddClick(Sender: TObject);
var
  AComponent: TComponent;
  AData: TdxAddReportLinkDlgData;
  I: Integer;
begin
  CheckAddLink;
  AData.Initialize;
  try
    AData.Options := [Low(TdxAddReportLinkDlgDataOption)..High(TdxAddReportLinkDlgDataOption)];
    AData.ReportLinkName := UniqueName(nil);
    AData.ReportLinkCaption := cxGetResourceString(@sdxNewReport);
    AData.ReportLinkCreator := dxGetUserName;
    AData.ComponentPrinter := TdxComponentPrinter(Controller);
    AData.Title := sdxAddReport;
    if dxShowAddComponentsDlg(AData) then
      for I := 0 to AData.Components.Count - 1 do
      begin
        AComponent := TComponent(AData.Components[I]);
        if (AData.ReportLinkName = '') or (AData.Components.Count <> 1) then
          AData.ReportLinkName := UniqueName(nil);
        InternalAddLink(TdxReportLinkClass(AData.ComponentsLinkClasses[I]), AComponent, AData.ReportLinkName,
          AData.ReportLinkCaption, AData.ReportLinkCreator, AData.ReportLinkDescription, AData.DesignBtnPressed);
      end;
  finally
    AData.Finalize;
  end;
end;

procedure TdxfmReportLinkDesignWindow.AddCompositionClick(Sender: TObject);
begin
  CheckAddLink;
  InternalAddLink(TdxCompositionReportLink, nil, UniqueName(nil), '', dxGetUserName, cxGetResourceString(@sdxComposition), False);
end;

procedure TdxfmReportLinkDesignWindow.AddStandardClick(Sender: TObject);
var
  Data: TdxAddReportLinkClassDlgData;
begin
  CheckAddLink;
  FillChar(Data, SizeOf(TdxAddReportLinkClassDlgData), 0);
  Data.ReportLinkName := UniqueName(nil);
  Data.ReportLinkCaption := sdxNewReport;
  Data.ReportLinkCreator := dxGetUserName;
  if dxShowAddReportLinkClassDlg(Data) then
  begin
    if Data.ReportLinkName = '' then
      Data.ReportLinkName := UniqueName(nil);
    InternalAddLink(Data.ReportLinkClass, nil, Data.ReportLinkName, Data.ReportLinkCaption,
      Data.ReportLinkCreator, Data.ReportLinkDescription, Data.DesignBtnPressed);
  end;
end;

procedure TdxfmReportLinkDesignWindow.AddExistingClick(Sender: TObject);
var
  Link: TBasedxReportLink;
begin
  CheckAddLink;
  Link := TBasedxReportLink(TComponent(Sender).Tag);
  if Link <> nil then
    InternalAddLink(Link.LinkClass, nil, UniqueName(Link),
      Link.ReportDocument.Caption, Link.ReportDocument.Creator,
      Link.ReportDocument.Description, False);
end;

procedure TdxfmReportLinkDesignWindow.lbxLinksClick(Sender: TObject);
var
  Selections: TdxDesignSelectionList;
begin
  Selections := CreateDesignSelectionList;
  GetSelections(Selections);
  Designer.SetSelections(Selections);
end;

procedure TdxfmReportLinkDesignWindow.LinkDesignClick(Sender: TObject);
begin
  CurrentLink.IsCurrentLink := True;
  if CurrentLink is TdxCompositionReportLink then
    ShowCollectionEditor(Designer, CurrentLink, TdxCompositionReportLink(CurrentLink).Items, 'Items')
  else
    if Controller.DesignReport(CurrentLink) then
      Designer.Modified;
end;

procedure TdxfmReportLinkDesignWindow.SetAsCurrentClick(Sender: TObject);
begin
  Controller.CurrentLink := CurrentLink;
end;

procedure TdxfmReportLinkDesignWindow.LinkChangeComponentClick(Sender: TObject);
var
  AData: TdxAddReportLinkDlgData;
begin
  AData.Initialize;
  try
    AData.ReportLink := CurrentLink;
    AData.ComponentPrinter := TdxComponentPrinter(Controller);
    AData.Options := [adoShowOnlyComponentsWOLinks..adoShowOnlyComponentsInActiveForm, adoShowHideCustomContainers];
    AData.Title := sdxChangeComponent;
    if dxShowAddComponentsDlg(AData) and (AData.Components.Count > 0) then
    begin
      CurrentLink.Component := TComponent(AData.Components[0]);
      Designer.Modified;
      ActiveControl := lbxLinks;
    end;
  finally
    AData.Finalize;
  end;
end;

procedure TdxfmReportLinkDesignWindow.RestoreDefaultsClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to LinkCount - 1 do
    if Selected[I] or (SelectedCount = 0) then
    begin
      Links[I].RestoreDefaults;
      Links[I].RealPrinterPage.RestoreDefaults;
    end;
end;

procedure TdxfmReportLinkDesignWindow.RestoreOriginalClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to LinkCount - 1 do
    if (Selected[I] or (SelectedCount = 0)) and Links[I].DataProviderPresent then
      Links[I].RestoreFromOriginal;
end;

procedure TdxfmReportLinkDesignWindow.PageSetupClick(Sender: TObject);
begin
  CurrentLink.IsCurrentLink := True;
  if CurrentLink.PageSetup then Designer.Modified;
end;

procedure TdxfmReportLinkDesignWindow.PrintPreviewClick(Sender: TObject);
begin
  CurrentLink.IsCurrentLink := True;
  dxShowPreviewWindow(Controller, Designer);
end;

procedure TdxfmReportLinkDesignWindow.PrintClick(Sender: TObject);
begin
  CurrentLink.IsCurrentLink := True;
  Controller.Print(True, nil, nil);
end;

procedure TdxfmReportLinkDesignWindow.lbxLinksDblClick(Sender: TObject);
begin
  if CanShowDesigner then
    LinkDesignClick(Sender)
  else
    if CanPrintPreview then
      PrintPreviewClick(Sender)
    else
      if CanPageSetup then
        PageSetupClick(Sender);
end;

procedure TdxfmReportLinkDesignWindow.lbxLinksStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  FSaveDragIndex := -1;
end;

procedure TdxfmReportLinkDesignWindow.lbxLinksEndDrag(Sender, Target: TObject;
  X, Y: Integer);
begin
  DrawDragRect;
end;

procedure TdxfmReportLinkDesignWindow.lbxLinksDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  Index: Integer;
begin
  Accept := Sender = cxExtractDragObjectSource(Source);
  if Accept then
    with TcxListBox(Sender) do
    begin
      Index := ItemAtPos(Point(X, Y), True);
      Accept := (Index <> -1) and (FSaveDragIndex <> ItemIndex);
      DrawDragRect;
      FSaveDragIndex := Index;
      DrawDragRect;
    end;
end;

procedure TdxfmReportLinkDesignWindow.lbxLinksDragDrop(Sender, Source: TObject;
  X, Y: Integer);
begin
  MoveSelection(FSaveDragIndex - TcxListBox(Sender).ItemIndex);
end;

procedure TdxfmReportLinkDesignWindow.lbxLinksKeyPress(Sender: TObject;
  var Key: Char);
begin
  case Key of
    #13, #33..#126:
      begin
        if Key = #13 then
          Key := #0;
        ActivateInspector(Key);
        Key := #0;
      end;
    #27:
      begin
        SelectController;
        Key := #0;
      end;
  end;
end;

procedure TdxfmReportLinkDesignWindow.EditClick(Sender: TObject);
begin
  EditAction(TEditAction(TComponent(Sender).Tag));
end;

procedure TdxfmReportLinkDesignWindow.MoveUpClick(Sender: TObject);
begin
  MoveSelection(-1);
end;

procedure TdxfmReportLinkDesignWindow.MoveDownClick(Sender: TObject);
begin
  MoveSelection(1);
end;

procedure TdxfmReportLinkDesignWindow.BackgroundClick(Sender: TObject);
var
  Background: TdxBackground;
  I: Integer;
begin
  StartWait;
  try
    Background := TdxBackground.Create;
    try
      for I := 0 to LinkCount - 1 do
        if Selected[I] or (SelectedCount = 0) then
        begin
          Background.Assign(Links[I].RealPrinterPage.Background);
          Break;
        end;
      if Background.SetupEffects then
      begin
        for I := 0 to LinkCount - 1 do
          if Selected[I] or (SelectedCount = 0) then
            Links[I].RealPrinterPage.Background := Background;
        Designer.Modified;
      end;
    finally
      Background.Free;
    end;
  finally
    StopWait;
  end;
end;

procedure TdxfmReportLinkDesignWindow.ClearBackgroundClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to LinkCount - 1 do
    if Selected[I] or (SelectedCount = 0) then
      Links[I].RealPrinterPage.Background.Clear;
  Designer.Modified;
end;

procedure TdxfmReportLinkDesignWindow.pmLinksPopup(Sender: TObject);
begin
  UpdateMenuState;
end;

procedure TdxfmReportLinkDesignWindow.ShowButtonsClick(Sender: TObject);
begin
  pnlButtons.Visible := not pnlButtons.Visible;
end;

function TdxfmReportLinkDesignWindow.EditAction(Action: TEditAction): Boolean;
begin
  Result := True;
  case Action of
    eaCut:
      Cut;
    eaCopy:
      Copy;
    eaPaste:
      Paste;
    eaDelete:
      Delete;
    eaSelectAll:
      SelectAll;
  end;
end;

function TdxfmReportLinkDesignWindow.GetEditState: TEditState;
begin
  Result := [];
  if CanCut then
    Result := Result + [esCanCut];
  if CanCopy then
    Result := Result + [esCanCopy];
  if CanPaste then
    Result := Result + [esCanPaste];
  if CanDelete then
    Result := Result + [esCanDelete];
  if CanSelectAll then
    Result := Result + [esCanSelectAll];
end;

procedure TdxfmReportLinkDesignWindow.ItemDeleted(const ADesigner: IDesigner; Item: TPersistent);
begin
  inherited ItemDeleted(ADesigner, Item);
  if (ADesigner = Designer) and (Item is TBasedxReportLink) and
    (Controller <> nil) and not (csDestroying in Controller.ComponentState) then
    DeleteItem(TBasedxReportLink(Item));
end;

procedure TdxfmReportLinkDesignWindow.ItemsModified(const Designer: IDesigner);
begin
  inherited ItemsModified(Designer);
  UpdateCaption;
end;

procedure TdxfmReportLinkDesignWindow.SelectionChanged(
  const ADesigner: IDesigner; const ASelection: IDesignerSelections);
begin
  inherited SelectionChanged(ADesigner, ASelection);
  if ADesigner = Designer then
    UpdateSelections(ASelection);
end;

procedure TdxfmReportLinkDesignWindow.Activated;
var
  Selections: TdxDesignSelectionList;
begin
  inherited Activated;
  Selections := CreateDesignSelectionList;
  try
    Designer.GetSelections(Selections);
    UpdateSelections(Selections);
  finally
    FreeDesignSelectionList(Selections);
  end;
end;

procedure TdxfmReportLinkDesignWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    Style := Style or WS_THICKFRAME;
  Params.WndParent := Application.MainForm.Handle;
end;

procedure TdxfmReportLinkDesignWindow.CreateWnd;
begin
  inherited CreateWnd;
  SendMessage(Handle, WM_SETICON, 1, Icon.Handle)
end;

function TdxfmReportLinkDesignWindow.UniqueName(Comp: TComponent): string;
begin
  Result := dxReportLinkUniqueName(Controller, Comp);
end;

function TdxfmReportLinkDesignWindow.GetControllerDesigner: TAbstractdxReportLinkDesigner;
begin
  if Controller <> nil then
    Result := Controller.ReportLinkDesigner
  else
    Result := nil;
end;

function TdxfmReportLinkDesignWindow.GetCurrentLink: TBasedxReportLink;
begin
  if LinkCount <> 0 then
    Result := Links[lbxLinks.ItemIndex]
  else
    Result := nil;
end;

function TdxfmReportLinkDesignWindow.GetLink(Index: Integer): TBasedxReportLink;
begin
  Result := TBasedxReportLink(lbxLinks.Items.Objects[Index]);
end;

function TdxfmReportLinkDesignWindow.GetLinkCount: Integer;
begin
  Result := lbxLinks.Items.Count;
end;

function TdxfmReportLinkDesignWindow.GetLinkDescription(Index: Integer): string;
var
  Link: TBasedxReportLink;
begin
  Link := Links[Index];
  if Link.Component <> nil then
    Result := sdxLinkedTo + ' ' + Link.Component.Name
  else
    if Link.DataProviderPresent then
      Result := sdxLinked
    else
      Result := sdxUnassigned;

  Result := '[' + Result + ']';
end;

function TdxfmReportLinkDesignWindow.GetRegistryPath: string;
begin
  Result := dxPSDsgProxies.GetBaseRegistryKey + '\' + sdxPSRegPathRunTimeFormLayouts + '\' + DropT(ClassName);
end;

function TdxfmReportLinkDesignWindow.GetSelected(Index: Integer): Boolean;
begin
  Result := lbxLinks.Selected[Index]
end;

function TdxfmReportLinkDesignWindow.GetSelectedCount: Integer;
begin
  Result := lbxLinks.SelCount;
end;

procedure TdxfmReportLinkDesignWindow.SetController(Value: TCustomdxComponentPrinter);
begin
  if FController <> Value then
  begin
    FController := Value;
    UpdateCaption;
    RefreshList;
  end;
end;

procedure TdxfmReportLinkDesignWindow.SetSelected(Index: Integer; Value: Boolean);
begin
  lbxLinks.Selected[Index] := Value;
end;

function TdxfmReportLinkDesignWindow.CalculateLinkCaptionWidth: Integer;
var
  I, W: Integer;
begin
  Result := 0;
  with lbxLinks do
    for I := 0 to Items.Count - 1 do
    begin
      if Links[I].IsCurrentLink then
        Canvas.Font.Style := Canvas.Font.Style + [fsBold];
      W := Canvas.TextWidth(Items[I]);
      Canvas.Font.Style := Canvas.Font.Style - [fsBold];
      if Result < W then Result := W;
    end;
end;

function TdxfmReportLinkDesignWindow.CalculateLinkDescriptionOffset: Integer;
begin
  Result := CalculateLinkCaptionWidth + 10;
end;

function TdxfmReportLinkDesignWindow.CalculateLinkDescriptionWidth: Integer;
var
  I, W: Integer;
begin
  Result := 0;
  with lbxLinks do
    for I := 0 to LinkCount - 1 do
    begin
      if Links[I].IsCurrentLink then
        Canvas.Font.Style := Canvas.Font.Style + [fsBold];
      W := Canvas.TextWidth(LinkDescriptions[I]);
      Canvas.Font.Style := Canvas.Font.Style - [fsBold];
      if Result < W then Result := W;
    end;
end;

function TdxfmReportLinkDesignWindow.CalculateLinkMaxRowWidth: Integer;
begin
  Result := CalculateLinkDescriptionOffset + CalculateLinkDescriptionWidth;
end;

function TdxfmReportLinkDesignWindow.CanAdd: Boolean;
begin
  Result := not ((Controller <> nil) and (Controller.Owner <> nil) and (csInline in Controller.Owner.ComponentState));
end;

function TdxfmReportLinkDesignWindow.CanAddExisting: Boolean;
begin
  Result := CanAdd and (CurrentLink <> nil);
end;

function TdxfmReportLinkDesignWindow.CanBackgroundClear: Boolean;
begin
  Result := LinkCount <> 0;
end;

function TdxfmReportLinkDesignWindow.CanBackgroundEffects: Boolean;
begin
  Result := LinkCount <> 0;
end;

function TdxfmReportLinkDesignWindow.CanChangeComponent: Boolean;
begin
  Result := (SelectedCount = 1) and not (CurrentLink is TdxCompositionReportLink);
end;

function TdxfmReportLinkDesignWindow.CanCopy: Boolean;
begin
  Result := SelectedCount <> 0;
end;

function TdxfmReportLinkDesignWindow.CanCut: Boolean;
begin
  Result := CanCopy and CanDelete;
end;

function TdxfmReportLinkDesignWindow.CanDelete: Boolean;
var
  I: Integer;
begin
  Result := SelectedCount <> 0;
  if Result then
    for I := 0 to LinkCount - 1 do
      if Selected[I] and (csAncestor in Links[I].ComponentState) then
      begin
        Result := False;
        Exit;
      end;
end;

function TdxfmReportLinkDesignWindow.CanMoveDown: Boolean;
var
  I, Counter: Integer;
begin
  Counter := 0;
  for I := LinkCount - 1 downto 0 do
  begin
    if not Selected[I] then
    begin
      Result := Counter < SelectedCount;
      Exit;
    end;
    Inc(Counter);
  end;
  Result := False;
end;

function TdxfmReportLinkDesignWindow.CanMoveUp: Boolean;
var
  I: Integer;
begin
  for I := 0 to LinkCount - 1 do
    if not Selected[I] then
    begin
      Result := I < SelectedCount;
      Exit;
    end;
  Result := False;
end;

function TdxfmReportLinkDesignWindow.CanPaste: Boolean;
begin
  Result := CanAdd and ClipboardComponents and Designer.CanPaste;
end;

function TdxfmReportLinkDesignWindow.CanPageSetup: Boolean;
begin
  Result := (SelectedCount = 1) and (CurrentLink <> nil) and (rlcPageSetup in CurrentLink.Capabilities);
end;

function TdxfmReportLinkDesignWindow.CanPrint: Boolean;
begin
  Result := (SelectedCount = 1) and (CurrentLink <> nil) and CurrentLink.DataProviderPresent;
end;

function TdxfmReportLinkDesignWindow.CanPrintPreview: Boolean;
begin
  Result := (SelectedCount = 1) and (CurrentLink <> nil) and CurrentLink.DataProviderPresent;
end;

function TdxfmReportLinkDesignWindow.CanRestoreDefaults: Boolean;
begin
  Result := CurrentLink <> nil;
end;

function TdxfmReportLinkDesignWindow.CanRestoreOriginal: Boolean;
begin
  Result := LinkCount > 0;
end;

function TdxfmReportLinkDesignWindow.CanSelectAll: Boolean;
begin
  Result := LinkCount <> SelectedCount;
end;

function TdxfmReportLinkDesignWindow.CanSetAsCurrent: Boolean;
begin
  Result := (SelectedCount = 1) and (CurrentLink <> nil) and not CurrentLink.IsCurrentLink;
end;

function TdxfmReportLinkDesignWindow.CanShowDesigner: Boolean;
begin
  Result := (SelectedCount = 1) and (CurrentLink <> nil) and CurrentLink.CheckToDesign;
end;

procedure TdxfmReportLinkDesignWindow.CheckAddLink;
begin
  if not CanAdd then
    raise EdxException.CreateRes(@SCantAddToFrame);
end;

procedure TdxfmReportLinkDesignWindow.CheckDeleteLink;
begin
  if not CanDelete then
    raise EdxException.CreateRes(@SCantDeleteAncestor);
end;

procedure TdxfmReportLinkDesignWindow.Copy;
var
  Components: TdxDesignSelectionList;
begin
  Components := CreateDesignSelectionList;
  try
    GetSelections(Components);
    CopyComponents(Controller.Owner, Components);
  finally
    FreeDesignSelectionList(Components);
  end;
  UpdateHScrollBar;
end;

procedure TdxfmReportLinkDesignWindow.Cut;
begin
  Copy;
  CheckDeleteLink;
  Delete;
end;

procedure TdxfmReportLinkDesignWindow.Delete;
var
  Selections: TdxDesignSelectionList;
  I, ItemIndex: Integer;
  Item: TComponent;
begin
  StartWait;
  try
    ControllerDesigner.BeginUpdate;
    try
      Selections := CreateDesignSelectionList;
      try
        GetSelections(Selections);
        ItemIndex := lbxLinks.ItemIndex;
        for I := 0 to Selections.Count - 1 do
        begin
          Item := TComponent(Selections[I]);
          if not (csAncestor in Item.ComponentState) and (Item is TBasedxReportLink) then
            Item.Free;
        end;
        if ItemIndex < 0 then
          ItemIndex := 0;
        if ItemIndex > LinkCount - 1 then
          ItemIndex := LinkCount - 1;
      finally
        FreeDesignSelectionList(Selections);
      end;
    finally
      ControllerDesigner.CancelUpdate;
    end;
    if ItemIndex <> -1 then
      Select(Links[ItemIndex], False)
    else
      SelectController;
    UpdateHScrollBar;
  finally
    StopWait;
  end;
end;

procedure TdxfmReportLinkDesignWindow.DeleteItem(AItem: TBasedxReportLink);
var
  Index, ItemIndex: Integer;
begin
  Index := IndexOf(AItem);
  if Index <> -1 then
  begin
    ItemIndex := lbxLinks.ItemIndex;
    lbxLinks.Items.Delete(Index);
    if ItemIndex < 0 then
      ItemIndex := 0;
    if ItemIndex > LinkCount - 1 then
      ItemIndex := LinkCount - 1;
    if ItemIndex <> -1 then
      Select(Links[ItemIndex], False)
    else
      SelectController;
    UpdateHScrollBar;
  end;
end;

procedure TdxfmReportLinkDesignWindow.DrawDragRect;
begin
  with lbxLinks do
    if (FSaveDragIndex <> -1) and (FSaveDragIndex <> ItemIndex) then
      DrawFocusRect(Canvas.Handle, ItemRect(FSaveDragIndex));
end;

function TdxfmReportLinkDesignWindow.GetMinWindowSize: TPoint;
begin
  Result.X := 300;
  Result.Y := btnPrint.Top + btnPrint.Height +
    GetSystemMetrics(SM_CYCAPTION) + 2 * GetSystemMetrics(SM_CXFRAME) + 4;
end;

procedure TdxfmReportLinkDesignWindow.GetSelections(const ASelections: TdxDesignSelectionList);
var
  I: Integer;
begin
  for I := 0 to LinkCount - 1 do
    if Selected[I] then
      ASelections.Add(Links[I]);
  if ASelections.Count = 0 then
    ASelections.Add(Controller);
end;

function TdxfmReportLinkDesignWindow.IndexOf(AItem: TBasedxReportLink): Integer;
begin
  Result := lbxLinks.Items.IndexOfObject(AItem);
end;

procedure TdxfmReportLinkDesignWindow.InternalAddLink(ALinkClass: TdxReportLinkClass;
  AComponent: TComponent; const AName, ACaption, ACreator, ADescription: string;
  AShowDesigner: Boolean);
var
  ALink: TBasedxReportLink;
begin
//  ALink := TBasedxReportLink(Designer.CreateComponent(ALinkClass, Designer.GetRoot, 0, 0, 0, 0));
  ControllerDesigner.BeginUpdate;
  try
    ALink := Controller.AddEmptyLinkEx(ALinkClass, Designer.GetRoot);
    ALink.Component := AComponent;
    ALink.Name := AName;
    if ACaption <> cxGetResourceString(@sdxNewReport) then
      ALink.ReportDocument.Caption := ACaption;
    ALink.ReportDocument.Creator := ACreator;
    ALink.ReportDocument.Description := ADescription;
  finally
    ControllerDesigner.CancelUpdate;
  end;

  if AComponent <> nil then
    Self.MakeLinkable(AComponent);

  lbxLinks.Items.AddObject(ALink.Name, ALink);
  Select(ALink, False);
  UpdateHScrollBar;
  lbxLinks.Update;

  if AShowDesigner then LinkDesignClick(nil);
end;

procedure TdxfmReportLinkDesignWindow.MakeLinkable(AComponent: TComponent);
begin
  with Designer do
    if (GetRoot.FindComponent(AComponent.Name) <> AComponent) and not IsComponentLinkable(AComponent) then
      MakeComponentLinkable(AComponent)
end;

procedure TdxfmReportLinkDesignWindow.MoveSelection(ADelta: Integer);

  procedure MoveDown(ADelta: Integer);
  var
    I, Index: Integer;
  begin
    for I := LinkCount - 1 downto 0 do
      if Selected[I] then
      begin
        Index := Links[I].Index;
        Inc(Index, ADelta);
        if Index > LinkCount - 1 then
          Index := LinkCount - 1;
//        while (Index < LinkCount) and Selected[Index] do
//          Inc(Index);
        Links[I].Index := Index;
      end;
  end;

  procedure MoveUp(ADelta: Integer);
  var
    I, Index: Integer;
  begin
    for I := 0 to LinkCount - 1 do
      if Selected[I] then
      begin
        Index := Links[I].Index;
        Inc(Index, ADelta);
        if Index < 0 then
          Index := 0;
//        while (Index > -1) and Selected[Index] do
//          Dec(Index);
        Links[I].Index := Index;
      end;
  end;

begin
  ControllerDesigner.BeginUpdate;
  try
    if ADelta > 0 then
      MoveDown(ADelta)
    else
      MoveUp(ADelta);
  finally
    ControllerDesigner.EndUpdate;
  end;
end;

procedure TdxfmReportLinkDesignWindow.Paste;
var
  Components: TdxDesignSelectionList;
  I: Integer;
begin
  Components := CreateDesignSelectionList;
  try
    StartWait;
    try
      lbxLinks.Items.BeginUpdate;
      try
        ControllerDesigner.BeginUpdate;
        try
          PasteComponents(Controller.Owner, Controller, Components);
        finally
          ControllerDesigner.EndUpdate;
        end;
        for I := LinkCount - 1 downto LinkCount - Components.Count do
          Selected[I] := True;
        Designer.SetSelections(Components);
      finally
        lbxLinks.Items.EndUpdate;
      end;
    finally
      StopWait;
    end;
  finally
    FreeDesignSelectionList(Components);
  end;
  UpdateHScrollBar;
end;

procedure TdxfmReportLinkDesignWindow.PrepareAddExistingItem(AMenuItem: TMenuItem);
begin
  if CanAddExisting then
  begin
    AMenuItem.Caption := 'Add ' + DropT(CurrentLink.ClassName);
    AMenuItem.Tag := Integer(CurrentLink);
  end;
end;

procedure TdxfmReportLinkDesignWindow.RefreshList;
var
  Selections: TdxDesignSelectionList;
  I, Index: Integer;
  Item: TBasedxReportLink;
  Component: TPersistent;
begin
  lbxLinks.Items.BeginUpdate;
  try
    Selections := CreateDesignSelectionList;
    try
      GetSelections(Selections);

      lbxLinks.Items.Clear;
      if Controller = nil then Exit;

      for I := 0 to Controller.LinkCount - 1 do
      begin
        Item := Controller.ReportLink[I];
        if Item.Owner = Controller.Owner then
          lbxLinks.Items.AddObject(Item.Name, Item);
      end;

      for I := 0 to Selections.Count - 1 do
      begin
        Component := Selections[I];
        if Component is TBasedxReportLink then
        begin
          Index := IndexOf(TBasedxReportLink(Component));
          if Index <> -1 then
            Selected[Index] := True;
        end;
      end;
    finally
      FreeDesignSelectionList(Selections);
    end;
  finally
    lbxLinks.Items.EndUpdate;
  end;
  UpdateHScrollBar;
end;

procedure TdxfmReportLinkDesignWindow.RestoreLayout;
begin
  with TRegistry.Create do
  try
    try
      if OpenKey(RegistryPath, False) then
      begin
        if ValueExists(sdxButtonBar) then
          pnlButtons.Visible := ReadBool(sdxButtonBar);
        if ValueExists(sdxWidth) then
          Width := ReadInteger(sdxWidth);
        if ValueExists(sdxHeight) then
          Height := ReadInteger(sdxHeight);
      end;
    except
      //
    end;
  finally
    Free;
  end;
end;

procedure TdxfmReportLinkDesignWindow.SaveLayout;
begin
  with TRegistry.Create do
  try
    try
      if OpenKey(RegistryPath, True) then
      begin
        WriteBool(sdxButtonBar, pnlButtons.Visible);
        WriteInteger(sdxWidth, Width);
        WriteInteger(sdxHeight, Height);
      end;
    except
      //
    end;
  finally
    Free;
  end;
end;

procedure TdxfmReportLinkDesignWindow.Select(AItem: TPersistent; AddToSelection: Boolean);
var
  Selections: TdxDesignSelectionList;
begin
  Selections := CreateDesignSelectionList;
  if AddToSelection then
    Designer.GetSelections(Selections);
  Selections.Add(AItem);
  Designer.SetSelections(Selections);
end;

procedure TdxfmReportLinkDesignWindow.SelectAll;
var
  Selections: TdxDesignSelectionList;
  I: Integer;
begin
  Selections := CreateDesignSelectionList;
  for I := 0 to LinkCount - 1 do
    Selections.Add(Links[I]);
  Designer.SetSelections(Selections);
end;

procedure TdxfmReportLinkDesignWindow.SelectController;
begin
  Select(Controller, False);
end;

procedure TdxfmReportLinkDesignWindow.StartWait;
begin
  FSaveCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
end;

procedure TdxfmReportLinkDesignWindow.StopWait;
begin
  Screen.Cursor := FSaveCursor;
end;

procedure TdxfmReportLinkDesignWindow.UpdateCaption;
var
  NewCaption: string;
begin
  if (Controller <> nil) and (Controller.Owner <> nil) then
    NewCaption := Format(sdxLinkDesigner, [Controller.Owner.Name, '.', Controller.Name]);
  if Caption <> NewCaption then
    Caption := NewCaption;
end;

procedure TdxfmReportLinkDesignWindow.UpdateControlsState;
begin
  btnAdd.Enabled := CanAdd;
  btnDelete.Enabled := CanDelete;
  btnSelectAll.Enabled := CanSelectAll;
  btnMoveUp.Enabled := CanMoveUp;
  btnMoveDown.Enabled := CanMoveDown;
  btnShowDesigner.Enabled := CanShowDesigner;
  btnChangeComponent.Enabled := CanChangeComponent;
  btnRestoreDefaults.Enabled := CanRestoreDefaults;
  btnRestoreOriginal.Enabled := CanRestoreOriginal;
  btnPageSetup.Enabled := CanPageSetup;
  btnPrintPreview.Enabled := CanPrintPreview;
  btnPrint.Enabled := CanPrint;
end;

procedure TdxfmReportLinkDesignWindow.UpdateHScrollBar;
begin
  lbxLinks.Perform(LB_SETHORIZONTALEXTENT, 2 + CalculateLinkMaxRowWidth + 2, 0);
end;

procedure TdxfmReportLinkDesignWindow.UpdateItem(AItem: TBasedxReportLink);
var
  Index: Integer;
begin
  if AItem <> nil then
  begin
    Index := IndexOf(AItem);
    if Index <> -1 then
    begin
      lbxLinks.Items[Index] := AItem.Name;
      UpdateHScrollBar;
    end;
  end
  else
    RefreshList;
  UpdateControlsState;
end;

procedure TdxfmReportLinkDesignWindow.UpdateMenuState;
begin
  miAdd.Enabled := CanAdd;
  miAdd1.Enabled := CanAdd;
  miAddComposition.Enabled := CanAdd;
  miAddComposition1.Enabled := CanAdd;
  miAddStandard.Visible := CanAdd;
  miAddStandard1.Visible := CanAdd;
  miAddExisting.Visible := CanAddExisting;
  miAddExisting1.Visible := CanAddExisting;
  PrepareAddExistingItem(miAddExisting);
  PrepareAddExistingItem(miAddExisting1);

  miCut.Enabled := CanCut;
  miCopy.Enabled := CanCopy;
  miPaste.Enabled := CanPaste;
  miDelete.Enabled := CanDelete;
  miSelectAll.Enabled := CanSelectAll;
  miMoveUp.Enabled := CanMoveUp;
  miMoveDown.Enabled := CanMoveDown;
  miShowDesigner.Enabled := CanShowDesigner;
  miSetAsCurrent.Enabled := CanSetAsCurrent;
  miChangeComponent.Enabled := CanChangeComponent;
  miRestoreDefaults.Enabled := CanRestoreDefaults;
  miRestoreOriginal.Enabled := CanRestoreOriginal;
  miPageSetup.Enabled := CanPageSetup;
  miPrintPreview.Enabled := CanPrintPreview;
  miPrint.Enabled := CanPrint;
  miBackgroundEffects.Enabled := CanBackgroundEffects;
  miBackgroundClear.Enabled := CanBackgroundClear;
  miShowButtons.Checked := pnlButtons.Visible;

  if CanShowDesigner then
    miShowDesigner.Default := True
  else
    if CanPrintPreview then
      miPrintPreview.Default := True
    else
      if CanPageSetup then
        miPageSetup.Default := True;
end;

procedure TdxfmReportLinkDesignWindow.UpdateSelections(const ASelections: TdxDesignSelectionList);

  function InSelection(ALink: TBasedxReportLink): Boolean;
  var
    I: Integer;
  begin
    for I := 0 to ASelections.Count - 1 do
      if ALink = ASelections[I] then
      begin
        Result := True;
        Exit;
      end;
    Result := False;
  end;

var
  I: Integer;
begin
  if (ASelections = nil) or (Controller = nil) or (csDestroying in Controller.ComponentState) or
    (ControllerDesigner = nil) or (TdxReportLinkDesigner(ControllerDesigner).FUpdateCount <> 0) then
    Exit;
  for I := 0 to LinkCount - 1 do
    if Selected[I] <> InSelection(Links[I]) then
      Selected[I] := not Selected[I];
  UpdateControlsState;
end;

procedure TdxfmReportLinkDesignWindow.WMAppCommand(var Message: TMessage);
begin
  with Message do
    case dxPSGlbl.GET_APPCOMMAND_LPARAM(lParam) of
      APPCOMMAND_COPY:
        begin
          Result := Ord(CanCopy);
          if Result = 1 then EditAction(eaCopy);
        end;

      APPCOMMAND_CUT:
        begin
          Result := Ord(CanCut);
          if Result = 1 then EditAction(eaCut);
        end;

      APPCOMMAND_PASTE:
        begin
          Result := Ord(CanPaste);
          if Result = 1 then EditAction(eaPaste);
        end;

      APPCOMMAND_PRINT:
        begin
          Result := Ord(CanPrint);
          if Result = 1 then
          begin
            CurrentLink.IsCurrentLink := True;
            Controller.Print(True, nil, nil);
          end;
        end;
    end;
  inherited;
end;

procedure TdxfmReportLinkDesignWindow.WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
begin
  inherited;
  if not (csReadingState in ControlState) then
    Message.MinMaxInfo^.ptMinTrackSize := GetMinWindowSize;
end;

procedure TdxfmReportLinkDesignWindow.WMNCCreate(var Message: TWMNCCreate);
const
  TypeDataSize = 32;
var
  SysMenu: HMENU;
  P: Pointer;
  S: array[0..TypeDataSize - 1] of Char;
  Info: TMenuItemInfo;
  ItemExist: Boolean;
begin
  SysMenu := GetSystemMenu(Handle, False);
  P := @S[0];
  Info.cbSize := SizeOf(Info) - SizeOf(HBITMAP);
  Info.fMask := MIIM_ID or MIIM_TYPE;
  Info.dwTypeData := P;
  Info.cch := TypeDataSize;
  ItemExist := GetMenuItemInfo(SysMenu, SC_SIZE, False, Info);
  inherited;
  if ItemExist then
    InsertMenuItem(SysMenu, 0, True, Info);
end;

procedure TdxfmReportLinkDesignWindow.WMNCDestroy(var Message: TWMNCCreate);
begin
  GetSystemMenu(Handle, True);
  inherited;
end;

procedure TdxfmReportLinkDesignWindow.lbxLinksDrawItem(
  AControl: TcxListBox; ACanvas: TcxCanvas; AIndex: Integer; ARect: TRect;
  AState: TOwnerDrawState);
var
  Link: TBasedxReportLink;
  S: string;
  ASavedTextColor, ASavedBrushColor: TColor;
begin
  Link := Links[AIndex];
  S := LinkDescriptions[AIndex];
  ACanvas.FillRect(ARect, clDefault);
  Inc(ARect.Left, 4);

  if Link.IsCurrentLink then
    ACanvas.Font.Style := ACanvas.Font.Style + [fsBold];
  ASavedBrushColor := ACanvas.Brush.Color;
  try
    ACanvas.Brush.Style := bsClear;
    ACanvas.DrawTexT(Link.Name, ARect, cxAlignVCenter);

    if S <> '' then
    begin
      Inc(ARect.Left, CalculateLinkDescriptionOffset);
      ASavedTextColor := ACanvas.Font.Color;
      try
        if not (odSelected in AState) then
          ACanvas.Font.Color := clBlue;
        ACanvas.DrawTexT(S, ARect, cxAlignVCenter);
      finally
        ACanvas.Font.Color := ASavedTextColor;
      end;
    end;
    ACanvas.Brush.Style := bsSolid;
    if Link.IsCurrentLink then
      ACanvas.Font.Style := Canvas.Font.Style - [fsBold];
  finally
    ACanvas.Brush.Color := ASavedBrushColor;
  end;
end;

procedure TdxfmReportLinkDesignWindow.lbxLinksMeasureItem(
  AControl: TcxListBox; AIndex: Integer; var Height: Integer);
begin
  with AControl.Canvas do
  begin
    Font.Style := Font.Style + [fsBold];
    Height := 2 + TextHeight(dxMeasurePattern) + 2;
    Font.Style := Font.Style - [fsBold];
  end;
end;

initialization
  TdxPSDesignTimedxComponentsProvider.Register;

finalization
  TdxPSDesignTimedxComponentsProvider.Unregister;

end.
