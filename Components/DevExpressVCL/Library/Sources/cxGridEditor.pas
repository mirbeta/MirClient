{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid                                       }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL            }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit cxGridEditor; // TODO: Add Level and View...

{$I cxVer.inc}

interface

uses
  DesignIntf, DesignWindows, ComponentDesigner,
  Windows, Classes, SysUtils, Controls, Graphics, Forms, cxClasses,
  StdCtrls, ExtCtrls, ComCtrls, Menus,
  cxControls, cxDesignWindows, cxGridStructureNavigator,
  cxGrid, cxGridLevel, cxGridCustomView, cxViewEditor,
  cxGridViewLayoutEditor, cxLookAndFeelPainters, cxButtons, cxPC, cxLookAndFeels,
  cxGraphics;

type
  TSelectionInfo = record
    Level: TcxGridLevel;
    View: TcxCustomGridView;
    MultiSelect: Boolean;
  end;

  TcxGridEditor = class(TcxDesignFormEditor)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    BClose: TcxButton;
    Panel4: TPanel;
    PLeft: TPanel;
    Splitter1: TSplitter;
    Panel7: TPanel;
    Panel8: TPanel;
    PMGridStructureControl: TPopupMenu;
    PMViews: TPopupMenu;
    Panel11: TPanel;
    Panel10: TPanel;
    PMViewList: TPopupMenu;
    miDeleteView: TMenuItem;
    N1: TMenuItem;
    miEditLayout: TMenuItem;
    Panel5: TPanel;
    Panel12: TPanel;
    Label1: TLabel;
    LSelectedView: TLabel;
    Panel13: TPanel;
    Panel14: TPanel;
    PageControl1: TcxPageControl;
    tsLevels: TcxTabSheet;
    PLevels: TPanel;
    Panel6: TPanel;
    BAddLevel: TcxButton;
    BDeleteLevel: TcxButton;
    TabSheet2: TcxTabSheet;
    PViews: TPanel;
    LBViews: TListBox;
    Panel9: TPanel;
    BAddView: TcxButton;
    BDeleteView: TcxButton;
    BEditView: TcxButton;
    Panel15: TPanel;
    PViewFrame: TPanel;
    miCustomizeEditFormLayout: TMenuItem;
    procedure BCloseClick(Sender: TObject);
    procedure BDeleteLevelClick(Sender: TObject);
    procedure BAddLevelClick(Sender: TObject);
    procedure LBViewsClick(Sender: TObject);
    procedure BAddViewClick(Sender: TObject);
    procedure BDeleteViewClick(Sender: TObject);
    procedure GridStructureControlKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure miEditLayoutClick(Sender: TObject);
    procedure miCustomizeEditFormLayoutClick(Sender: TObject);
  private
    FGridStructureControl: TcxGridStructureControl;
    FGridStructureHelper: TcxGridStructureHelper;
    FViewEditor: TcxViewEditor;
    procedure CreateViewClick(Sender: TObject);
    function GetGrid: TcxCustomGrid;
    function GetView(Index: Integer): TcxCustomGridView;
    function GetViewCount: Integer;
    function GetViewRepository: TcxGridViewRepository;
    procedure GridStructureControlSelectionChanged(Sender: TObject);
    procedure HideViewFrame;
    procedure ShowViewFrame(AView: TcxCustomGridView; AMultiView: Boolean; ARefreshNeeded: Boolean);
    procedure UpdateButtons;
    procedure UpdateDesigner(Sender: TObject);
    procedure UpdateGridStructureControl;
    procedure UpdateGridStructureControlSelection;
    procedure UpdateViewFrame(ARefreshNeeded: Boolean);
    procedure UpdateViewFrameCaption;
    procedure UpdateViewList;
  protected
    function GetSelectionInfo: TSelectionInfo;
    function GetViewEditorClass(AView: TcxCustomGridView): TcxViewEditorClass; virtual;
    procedure InitFormEditor; override;
    procedure InitIDESelection; virtual;
    procedure InitLookAndFeel(AControl: TWinControl);
    procedure LoadSettings;
    procedure SaveSettings;
    function UniqueName(Component: TComponent): string; override;
    procedure UpdateContent; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoItemsModified; override;
    function GetRegKey: string;
    function GetRegSectionName: string;
    procedure SelectionsChanged(const ASelection: TDesignerSelectionList); override;
    property Grid: TcxCustomGrid read GetGrid;
    property ViewCount: Integer read GetViewCount;
    property ViewRepository: TcxGridViewRepository read GetViewRepository;
    property Views[Index: Integer]: TcxCustomGridView read GetView;
  end;

procedure ShowGridEditor(ADesigner: IDesigner; AGrid: TcxCustomGrid);
procedure ShowViewRepositoryEditor(ADesigner: IDesigner; AViewRepository: TcxGridViewRepository);

implementation

uses
  Messages, Registry, cxGridTableView;

{$R *.dfm}

procedure ShowGridEditor(ADesigner: IDesigner; AGrid: TcxCustomGrid);
begin
  ShowFormEditorClass(ADesigner, AGrid, TcxGridEditor);
end;

procedure ShowViewRepositoryEditor(ADesigner: IDesigner; AViewRepository: TcxGridViewRepository);
begin
  ShowFormEditorClass(ADesigner, AViewRepository, TcxGridEditor);
end;

{ TcxGridEditor }

constructor TcxGridEditor.Create(AOwner: TComponent);
var
  P: TPoint;
begin
  inherited;
  P := ActiveDesigner.Environment.GetWorkspaceOrigin;
  Left := P.X + 10;
  Top := P.Y + 10;
end;

destructor TcxGridEditor.Destroy;
begin
  FreeAndNil(FGridStructureHelper);
  inherited Destroy;
end;

procedure TcxGridEditor.DoItemsModified;
begin
  inherited;
  UpdateViewList;
  UpdateViewFrame(True);
end;

function TcxGridEditor.GetRegKey: string;
begin
  Result := GetBaseRegKey(ComponentDesigner) + '\TcxGridEditor';
end;

function TcxGridEditor.GetRegSectionName: string;
begin
  Result := ComponentClassName;
end;

procedure TcxGridEditor.SelectionsChanged(const ASelection: TDesignerSelectionList);
begin
  if Closing then Exit;
  UpdateGridStructureControlSelection;
  ListBoxSynchronizeSelection(LBViews);
  UpdateButtons;
  UpdateViewFrame(False);
end;

function TcxGridEditor.GetSelectionInfo: TSelectionInfo;

  procedure FindView(ASelection: TDesignerSelectionList; var AInfo: TSelectionInfo);
  var
    I: Integer;
  begin
    for I := 0 to ASelection.Count - 1 do
      if (ASelection[I] is TcxCustomGridView) and
        IsViewLinkedToComponent(TcxCustomGridView(ASelection[I]), Component) then
        if AInfo.View = nil then
          AInfo.View := TcxCustomGridView(ASelection[I])
        else
        begin
          AInfo.View := nil;
          AInfo.MultiSelect := True;
          Break;
        end;
  end;

  procedure FindLevel(ASelection: TDesignerSelectionList; var AInfo: TSelectionInfo);
  var
    I: Integer;
    ALevel: TcxGridLevel;
  begin
    for I := 0 to ASelection.Count - 1 do
      if ASelection[I] is TcxGridLevel then
      begin
        ALevel := TcxGridLevel(ASelection[I]);
        if ALevel.Control = Grid then
          if (AInfo.Level = nil) and (AInfo.View = nil) then
            if ALevel.GridView = nil then
              AInfo.Level := ALevel
            else
              AInfo.View := ALevel.GridView
          else
          begin
            AInfo.Level := nil;
            AInfo.View := nil;
            AInfo.MultiSelect := True;
            Break;
          end;
      end;
  end;

  procedure FindViewItem(ASelection: TDesignerSelectionList; var AInfo: TSelectionInfo);
  var
    I: Integer;
    AView: TcxCustomGridView;
  begin
    for I := 0 to ASelection.Count - 1 do
    begin
      AView := GetLinkedView(ASelection[I]);
      if (AView <> nil) and IsViewLinkedToComponent(AView, Component) then
        if AInfo.View = nil then
          AInfo.View := AView
        else
          if AInfo.View <> AView then
          begin
            AInfo.View := nil;
            AInfo.MultiSelect := True;
            Break;
          end;
    end;
  end;

var
  ASelectionList: TDesignerSelectionList;
begin
  Result.Level := nil;
  Result.View := nil;
  Result.MultiSelect := False;

  ASelectionList := CreateDesignerSelectionList;
  try
    Designer.GetSelections(ASelectionList);
    FindView(ASelectionList, Result);
    if (Result.View = nil) and not Result.MultiSelect then
    begin
      FindLevel(ASelectionList, Result);
      if (Result.Level = nil) and (Result.View = nil) and not Result.MultiSelect then
        FindViewItem(ASelectionList, Result);
    end;
  finally
    DeleteDesignerSelectionList(ASelectionList);
  end;

  if (Result.Level <> nil) and Result.Level.IsDestroying then
    Result.Level := nil;
  if (Result.View <> nil) and Result.View.IsDestroying then
    Result.View := nil;
end;

function TcxGridEditor.GetViewEditorClass(AView: TcxCustomGridView): TcxViewEditorClass;
begin
  Result := cxViewEditor.GetViewEditorClass(TcxCustomGridViewClass(AView.ClassType));
end;

procedure TcxGridEditor.InitFormEditor;
begin
  inherited InitFormEditor;
  FGridStructureControl := TcxGridStructureControl.Create(Self);
  with FGridStructureControl do
  begin
    Align := alClient;
    Grid := Self.Grid;
    MayFocused := True;
    MultiSelect := True;
    Keys := [kChars];
    OnKeyPress := GridStructureControlKeyPress;
    OnSelectionChanged := GridStructureControlSelectionChanged;
    Parent := PLevels;
  end;
  FGridStructureHelper := TcxGridStructureHelper.Create(FGridStructureControl);
  FGridStructureHelper.OnUpdateDesigner := UpdateDesigner;
  tsLevels.TabVisible := Grid <> nil;
  LSelectedView.Caption := '';
  LSelectedView.Font.Style := [fsBold];
  InitLookAndFeel(Self);
  MakeColoredControlsOpaque(Self);

  UpdateViewList;
  InitIDESelection;
  UpdateSelection;
  LoadSettings;
end;

procedure TcxGridEditor.InitIDESelection;
var
  ASelectionInfo: TSelectionInfo;
begin
  if (Grid = nil) or (Grid.ActiveLevel = nil) {or Grid.ActiveLevel.IsMaster} then Exit;
  ASelectionInfo := GetSelectionInfo;
  if (ASelectionInfo.Level = nil) and (ASelectionInfo.View = nil) and not ASelectionInfo.MultiSelect then
    if Grid.ActiveView = nil then
      SelectComponent(Grid.ActiveLevel)
    else
      SelectComponent(Grid.ActiveView);
end;

procedure TcxGridEditor.InitLookAndFeel(AControl: TWinControl);
begin
  SetControlLookAndFeel(AControl, lfUltraFlat, False);
end;

procedure TcxGridEditor.LoadSettings;
begin
  with TRegIniFile.Create(GetRegKey) do
    try
      Left := ReadInteger(GetRegSectionName, 'Left', Left);
      Top := ReadInteger(GetRegSectionName, 'Top', Top);
      Width := ReadInteger(GetRegSectionName, 'Width', Width);
      Height := ReadInteger(GetRegSectionName, 'Height', Height);
      PLeft.Width := ReadInteger(GetRegSectionName, 'PLeft.Width', PLeft.Width);
      // TODO: splitters
    finally
      Free;
    end;
  MakeVisibleOnDesktop(Self);
end;

procedure TcxGridEditor.SaveSettings;
begin
  with TRegIniFile.Create(GetRegKey) do
    try
      EraseSection(GetRegSectionName);
      WriteInteger(GetRegSectionName, 'Left', Left);
      WriteInteger(GetRegSectionName, 'Top', Top);
      WriteInteger(GetRegSectionName, 'Width', Width);
      WriteInteger(GetRegSectionName, 'Height', Height);
      WriteInteger(GetRegSectionName, 'PLeft.Width', PLeft.Width);
      // TODO: splitters
    finally
      Free;
    end;
end;

function TcxGridEditor.UniqueName(Component: TComponent): string;
begin
  if Component is TcxGridLevel then
  begin
    if Grid <> nil then
      Result := GenLevelName(Grid, Component as TcxGridLevel)
    else
      Result := inherited UniqueName(Component); // TODO
  end
  else
    if Component is TcxCustomGridView then
      Result := GenViewName(Self.Component, Component as TcxCustomGridView)
    else
      Result := inherited UniqueName(Component); // TODO
end;

procedure TcxGridEditor.UpdateContent;
begin
  inherited UpdateContent;
  UpdateGridStructureControl;
  UpdateViewList;
  UpdateButtons;
  UpdateViewFrame(True);
end;

procedure TcxGridEditor.CreateViewClick(Sender: TObject);
var
  AViewClass: TcxCustomGridViewClass;
  AView: TcxCustomGridView;
begin
  AViewClass := TcxCustomGridViewClass(
    cxGridRegisteredViews[((Sender as TMenuItem).Tag)]);
  FGridStructureControl.BeginUpdate;
  try
    ListBoxClearSelection(LBViews);
    if Grid <> nil then
      AView := Grid.CreateView(AViewClass)
    else
      AView := ViewRepository.CreateItem(AViewClass);
    AView.Name := GenViewName(Component, AView);
    RestoreViewFromTemplate(GetViewTemplateRegKey, AView);
    UpdateViewList;
    ListBoxSelectByObject(LBViews, AView);
    UpdateDesigner(nil);
  finally
    FGridStructureControl.EndUpdate;
  end;
  LBViewsClick(nil);
end;

procedure TcxGridEditor.miCustomizeEditFormLayoutClick(Sender: TObject);
var
  ATableView: TcxGridTableView;
begin
  ATableView := TcxGridTableView(GetSelectionInfo.View);
  ATableView.Controller.ShowEditFormCustomizationDialog;
end;

function TcxGridEditor.GetGrid: TcxCustomGrid;
begin
  if Component is TcxCustomGrid then
    Result := TcxCustomGrid(Component)
  else
    Result := nil;
end;

function TcxGridEditor.GetView(Index: Integer): TcxCustomGridView;
begin
  if Grid <> nil then
    Result := Grid.Views[Index]
  else
    Result := ViewRepository[Index];
end;

function TcxGridEditor.GetViewCount: Integer;
begin
  if Grid <> nil then
    Result := Grid.ViewCount
  else
    Result := ViewRepository.Count;
end;

function TcxGridEditor.GetViewRepository: TcxGridViewRepository;
begin
  if Component is TcxGridViewRepository then
    Result := TcxGridViewRepository(Component)
  else
    Result := nil;
end;

procedure TcxGridEditor.GridStructureControlKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    #13, #33..#126:
      begin
        if Key = #13 then Key := #0;
        ActivateInspector(Key);
        Key := #0;
      end;
  end;
end;

procedure TcxGridEditor.GridStructureControlSelectionChanged(Sender: TObject);
var
  AList: TList;
begin
  BeginUpdate;
  try
    AList := TList.Create;
    try
      FGridStructureControl.GetSelection(AList);
      SelectComponents(AList, Component.Owner);
    finally
      AList.Free;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TcxGridEditor.HideViewFrame;
begin
  if FViewEditor <> nil then
  begin
    FViewEditor.PViewEditor.Parent := FViewEditor;
    cxReleaseForm(FViewEditor);
  end;
end;

procedure TcxGridEditor.ShowViewFrame(AView: TcxCustomGridView; AMultiView: Boolean;
  ARefreshNeeded: Boolean);

  function IsExist(AView: TcxCustomGridView): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to ViewCount - 1 do
      if Views[I] = AView then
      begin
        Result := True;
        Break;
      end;
  end;

var
  AViewEditorClass: TcxViewEditorClass;
begin
  if (FViewEditor <> nil) and
    (AView <> nil) and (FViewEditor.ClassType = GetViewEditorClass(AView)) and
    (FViewEditor.View = AView) then
    FViewEditor.SetView(AView, ARefreshNeeded)
  else
  begin
    SendMessage(PViewFrame.Handle, WM_SETREDRAW, 0, 0);
    try
      HideViewFrame;
      if AView <> nil then
      begin
        AViewEditorClass := GetViewEditorClass(AView);
        if AViewEditorClass <> nil then
        begin
          FViewEditor := AViewEditorClass.Create(Self);
          InitLookAndFeel(FViewEditor);
          FViewEditor.FormEditor := Self;
          FViewEditor.SetView(AView, True);
          FViewEditor.PViewEditor.Parent := PViewFrame;
          MakeColoredControlsOpaque(PViewFrame);
        end;
      end;
    finally
      SendMessage(PViewFrame.Handle, WM_SETREDRAW, 1, 0);
      cxRedrawWindow(PViewFrame.Handle, RDW_INVALIDATE or RDW_ERASE or RDW_ALLCHILDREN);
    end;
  end;
  UpdateViewFrameCaption;
end;

procedure TcxGridEditor.UpdateButtons;

  function GetSelectedViewObject: TObject;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to LBViews.Count - 1 do
      if LBViews.Selected[I] then
      begin
        Result := LBViews.Items.Objects[I];
        Break;
      end;
  end;

var
  ASelectionLevelCount: Integer;
  AIntf: IcxGridViewLayoutEditorSupport;
begin
  ASelectionLevelCount := FGridStructureControl.GetSelectionLevelCount;
  BAddLevel.Enabled := CanAddComponent and (ASelectionLevelCount <= 1);
  BDeleteLevel.Enabled := CanDeleteComponent(nil) and (ASelectionLevelCount > 0);
  BAddView.Enabled := CanAddComponent;
  BDeleteView.Enabled := CanDeleteComponent(nil) and (LBViews.SelCount > 0);
  BEditView.Enabled := (LBViews.SelCount = 1) and
    Supports(GetSelectedViewObject, IcxGridViewLayoutEditorSupport, AIntf) and
    AIntf.CanEditViewLayoutAndData;
  miDeleteView.Enabled := BDeleteView.Enabled;
  miEditLayout.Enabled := BEditView.Enabled;
  miCustomizeEditFormLayout.Visible := (LBViews.SelCount = 1) and
    (GetSelectedViewObject is TcxGridTableView) and
    TcxGridTableView(GetSelectedViewObject).IsInplaceEditFormMode;
end;

procedure TcxGridEditor.UpdateDesigner(Sender: TObject);
begin
  Designer.Modified;
end;

procedure TcxGridEditor.UpdateGridStructureControl;
begin
  FGridStructureControl.Changed;
end;

procedure TcxGridEditor.UpdateGridStructureControlSelection;
var
  AList: TList;
begin
  AList := TList.Create;
  try
    GetSelectionList(AList);
    FGridStructureControl.SyncSelection(AList);
  finally
    AList.Free;
  end;
end;

procedure TcxGridEditor.UpdateViewFrame(ARefreshNeeded: Boolean);
var
  ASelectionInfo: TSelectionInfo;
begin
  ASelectionInfo := GetSelectionInfo;
  ShowViewFrame(ASelectionInfo.View, ASelectionInfo.MultiSelect, ARefreshNeeded);
end;

procedure TcxGridEditor.UpdateViewFrameCaption;
begin
  if FViewEditor <> nil then
    LSelectedView.Caption := FViewEditor.View.Name
  else
    LSelectedView.Caption := 'none';
end;

procedure TcxGridEditor.UpdateViewList;
var
  I, AItemIndex, ATopIndex: Integer;
  ASelection: TStringList;
begin
  ListBoxSaveSelection(LBViews, ASelection, AItemIndex, ATopIndex);
  try
    LBViews.Items.Clear;
    for I := 0 to ViewCount - 1 do
      LBViews.Items.AddObject(Views[I].Name, Views[I]);
  finally
    ListBoxRestoreSelection(LBViews, ASelection, AItemIndex, ATopIndex);
  end;
end;

procedure TcxGridEditor.BCloseClick(Sender: TObject);
begin
  Close;
end;

// Levels

procedure TcxGridEditor.BAddLevelClick(Sender: TObject);
var
  ALevel: TcxGridLevel;
begin
  if Grid = nil then Exit;
  FGridStructureControl.BeginUpdate;
  try
    ALevel := FGridStructureControl.GetSelectedLevel;
    if ALevel <> nil then
      ALevel := ALevel.Add
    else
      ALevel := Grid.Levels.Add;
    ALevel.Name := GenLevelName(Grid, ALevel);
    UpdateDesigner(nil);
  finally
    FGridStructureControl.EndUpdate;
  end;
  GridStructureControlSelectionChanged(nil);
end;

function LevelsCompare(
  AItem1, AItem2: Pointer): Integer;
var
  ALevel1, ALevel2: TcxGridLevel;
begin
  ALevel1 := TcxGridLevel(AItem1);
  ALevel2 := TcxGridLevel(AItem2);
  Result := ALevel1.Level - ALevel2.Level;
  if Result = 0 then
  begin
    if not ALevel1.IsTop then
      Result := LevelsCompare(ALevel1.Parent, ALevel2.Parent);
    if Result = 0 then
      Result := ALevel1.Index - ALevel2.Index;
  end;
end;

procedure TcxGridEditor.BDeleteLevelClick(Sender: TObject);
var
  AList: TList;
  ALevel: TcxGridLevel;
  I: Integer;
begin
  AList := TList.Create;
  try
    FGridStructureControl.GetSelectionLevels(AList);
    AList.Sort(LevelsCompare);
    for I := AList.Count - 1 downto 0 do
    begin
      ALevel := TcxGridLevel(AList[I]);
      if not CanDeleteComponent(ALevel) then
        AList.Delete(I);
    end;
    if AList.Count > 0 then
    begin
      FGridStructureControl.BeginUpdate;
      try
        for I := AList.Count - 1 downto 0 do
          TcxGridLevel(AList[I]).Free;
        UpdateDesigner(nil);
      finally
        FGridStructureControl.EndUpdate;
      end;
    end;
  finally
    AList.Free;
  end;
  GridStructureControlSelectionChanged(nil);
end;

// Views

procedure TcxGridEditor.LBViewsClick(Sender: TObject);
begin
  ListBoxApplySelection(LBViews, Component);
end;

procedure TcxGridEditor.BAddViewClick(Sender: TObject);
var
  P: TPoint;
begin
  // TODO: cxButton
  PMViews.Items.Clear;
  FillRegisteredViewsMenu(PMViews.Items, CreateViewClick);
  P.X := 0;
  P.Y := BAddView.Height;
  P := BAddView.ClientToScreen(P);
  PMViews.Popup(P.X, P.Y);
end;

procedure TcxGridEditor.BDeleteViewClick(Sender: TObject);
begin
  if LBViews.SelCount > 0 then
  begin
    BeginUpdate;
    try
      ListBoxDeleteSelection(LBViews, True);
      UpdateViewList;
      LBViewsClick(nil);
      UpdateDesigner(nil);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TcxGridEditor.miEditLayoutClick(Sender: TObject);
var
  ASelectionInfo: TSelectionInfo;
begin
  ASelectionInfo := GetSelectionInfo;
  if not ASelectionInfo.MultiSelect and (ASelectionInfo.View <> nil) then
    if ShowGridViewEditor(ASelectionInfo.View) then
      UpdateDesigner(nil);
end;

procedure TcxGridEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  SaveSettings;
  inherited;
end;

end.
