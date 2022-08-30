unit uMain;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls, ImgList,
  ActnList, Menus, StdCtrls, MidasLib,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer,
  cxEdit, dxLayoutcxEditAdapters, dxLayoutControlAdapters, cxClasses, dxLayoutLookAndFeels, cxImageList,
  dxLayoutContainer, cxButtons, cxTextEdit, dxLayoutControl, cxSplitter, ComCtrls, cxTreeView, uBaseDemoForm,
  cxCheckBox, dxToggleSwitch, cxMemo, cxRichEdit, EMFCustomTutorial;

const
  EMFTreeViewRootID = 100;
  EMFTreeViewTutorialEntityCreationGroupID = 1050;
  EMFTreeViewTutorialEntityDeletingGroupID = 1100;
  EMFTreeViewTutorialEntityModificationGroupID = 1150;
  EMFTreeViewTutorialEntityQueryingGroupID = 1200;

type
  { TDemoUnitInfo }

  TDemoUnitInfo = class
  strict private
    FUnitClass: TBaseDemoFormClass;
    FUnitInstance: TBaseDemoForm;
  strict protected
    function GetCaption: string; virtual;
    function GetID: Integer; virtual;
    function GetParentID: Integer; virtual;
    function GetSortIndex: Integer; virtual;
  public
    constructor Create(AClass: TBaseDemoFormClass);
    destructor Destroy; override;

    procedure SetParent(AParent: TWinControl);

    property Caption: string read GetCaption;
    property ID: Integer read GetID;
    property ParentID: Integer read GetParentID;
    property SortIndex: Integer read GetSortIndex;
    property UnitClass: TBaseDemoFormClass read FUnitClass;
    property UnitInstance: TBaseDemoForm read FUnitInstance;
  end;

  { TDemoUnitGroupInfo }

  TDemoUnitGroupInfo = class(TDemoUnitInfo)
  strict private
    FCaption: string;
    FID: Integer;
    FParentID: Integer;
    FSortIndex: Integer;
  strict protected
    function GetCaption: string; override;
    function GetID: Integer; override;
    function GetParentID: Integer; override;
    function GetSortIndex: Integer; override;
  public
    constructor Create(const ACaption: string; AID, AParentID, ASortIndex: Integer); reintroduce;
  end;

  { TfrmMainBase }

  TfrmMain = class(TForm)
    cxTreeView1: TcxTreeView;
    cxLookAndFeelController1: TcxLookAndFeelController;
    dxLayoutControl1Group_Root: TdxLayoutGroup;
    dxLayoutControl1: TdxLayoutControl;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    pnlDemoSite: TPanel;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    cxImageList1: TcxImageList;
    tsShowCode: TdxToggleSwitch;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;

    procedure actExitExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cxTreeView1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tsShowCodePropertiesEditValueChanged(Sender: TObject);
  strict private
    FDemoUnit: TDemoUnitInfo;
    function GetActiveFrameID: Integer;
    function FindDemoByID(ID: Integer; out ADemo: TDemoUnitInfo): Boolean;
    procedure SetActiveFrameID(const AValue: Integer);
    procedure SetDemoUnit(const AValue: TDemoUnitInfo);

    function GetDemoCaption: string;
    procedure CheckShowCode;
    procedure PopulateTreeView;

    function GetMainFormCaption: string;

    property ActiveFrameID: Integer read GetActiveFrameID write SetActiveFrameID;
    property DemoUnit: TDemoUnitInfo read FDemoUnit write SetDemoUnit;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

procedure RegisterDemo(ADemoClass: TBaseDemoFormClass);

implementation

{$R *.dfm}

uses
  Math, Generics.Defaults, Generics.Collections, dxSplashUnit;

const
  StartFrameID = 4;

type
  TdxToggleSwitchPropertiesAccess = class(TdxToggleSwitchProperties);

var
  FDemoUnits: TObjectList<TDemoUnitInfo>;

procedure RegisterDemo(ADemoClass: TBaseDemoFormClass);
var
  ADemoInfo: TDemoUnitInfo;
begin
  ADemoInfo := TDemoUnitInfo.Create(ADemoClass);
  ADemoInfo.SetParent(nil);
  FDemoUnits.Add(ADemoInfo);
 end;

{ TDemoUnitGroupInfo }

constructor TDemoUnitGroupInfo.Create(const ACaption: string; AID, AParentID, ASortIndex: Integer);
begin
  inherited Create(nil);
  FCaption := ACaption;
  FID := AID;
  FParentID := AParentID;
  FSortIndex := ASortIndex;
end;

function TDemoUnitGroupInfo.GetCaption: string;
begin
  Result := FCaption;
end;

function TDemoUnitGroupInfo.GetID: Integer;
begin
  Result := FID;
end;

function TDemoUnitGroupInfo.GetParentID: Integer;
begin
  Result := FParentID;
end;

function TDemoUnitGroupInfo.GetSortIndex: Integer;
begin
  Result := FSortIndex;
end;

{ TDemoUnitInfo }

constructor TDemoUnitInfo.Create(AClass: TBaseDemoFormClass);
begin
  inherited Create;
  FUnitClass := AClass;
end;

destructor TDemoUnitInfo.Destroy;
begin
  SetParent(nil);
  inherited Destroy;
end;

function TDemoUnitInfo.GetCaption: string;
begin
  Result := UnitClass.GetCaption;
end;

function TDemoUnitInfo.GetID: Integer;
begin
  Result := UnitClass.GetID;
end;

function TDemoUnitInfo.GetParentID: Integer;
begin
  Result := UnitClass.GetGroupID;
end;

function TDemoUnitInfo.GetSortIndex: Integer;
begin
  Result := UnitClass.GetSortIndex;
end;

procedure TDemoUnitInfo.SetParent(AParent: TWinControl);
begin
  if (AParent <> nil) and (UnitInstance = nil) then
  begin
    FUnitInstance := UnitClass.Create(nil);
    FUnitInstance.Visible := False;
  end;
  if FUnitInstance <> nil then
    FUnitInstance.Parent := AParent;
  if AParent = nil then
    FreeAndNil(FUnitInstance)
  else
  begin
    FUnitInstance.SendToBack;
    FUnitInstance.Visible := True;
    FUnitInstance.BringToFront;
    FUnitInstance.Update;
  end;
end;

{ TfrmMainBase }

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  RootLookAndFeel.AddChangeListener(Self);
end;

procedure TfrmMain.cxTreeView1Click(Sender: TObject);
var
  ASelectedNode: TTreeNode;
begin
  ASelectedNode := cxTreeView1.Selected;
  if (ASelectedNode <> nil) and (ASelectedNode.Data <> nil) and (TObject(ASelectedNode.Data).ClassType <> TDemoUnitGroupInfo) then
    ActiveFrameID := TDemoUnitInfo(ASelectedNode.Data).ID;
end;

destructor TfrmMain.Destroy;
begin
  RootLookAndFeel.RemoveChangeListener(Self);
  inherited Destroy;
end;

procedure TfrmMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

function TfrmMain.GetMainFormCaption: string;
begin
  Result := 'ExpressEntityMapping Framework Tutorials';
end;

function TfrmMain.GetDemoCaption: string;
begin
  if DemoUnit <> nil then
    Result := DemoUnit.Caption
  else
    Result := Application.Title;
end;

procedure TfrmMain.CheckShowCode;
begin
  if (DemoUnit <> nil) and (DemoUnit.UnitInstance <> nil) then
    (DemoUnit.UnitInstance as TfrmEMFCustomTutorial).dxUIAdornerManager1.Guides.Active := tsShowCode.Checked;
end;

function TutorialSortindProc(Node1, Node2: TTreeNode; Data: Longint): Integer; stdcall;
var
  AUnit1, AUnit2: TDemoUnitInfo;
begin
  if Node1 <> nil then
    AUnit1 := TDemoUnitInfo(Node1.Data)
  else
    Exit(0);
  if Node2 <> nil then
    AUnit2 := TDemoUnitInfo(Node2.Data)
  else
    Exit(0);
  Result := CompareValue(AUnit1.SortIndex, AUnit2.SortIndex);
end;

procedure TfrmMain.PopulateTreeView;
var
  I, J: Integer;
  ARoot, AChild: TTreeNode;
  ADemoUnitInfo: TDemoUnitInfo;
begin
  cxTreeView1.Items.Clear;
  cxTreeView1.SortType := stNone;
  ARoot := cxTreeView1.Items.AddObjectFirst(nil, FDemoUnits[0].Caption,  FDemoUnits[0]);
  for I := 0 to FDemoUnits.Count - 1 do
  begin
    ADemoUnitInfo := FDemoUnits[I];
    for J := 0 to cxTreeView1.Items.Count - 1 do
      if TDemoUnitInfo(cxTreeView1.Items[J].Data).ID = ADemoUnitInfo.ParentID then
      begin
        if not (ADemoUnitInfo is TDemoUnitGroupInfo) then
        begin
          AChild := cxTreeView1.Items.AddChildObject(cxTreeView1.Items[J], ADemoUnitInfo.Caption,  ADemoUnitInfo);
          AChild.ImageIndex := 1;
          AChild.SelectedIndex := AChild.ImageIndex;
        end
        else
          cxTreeView1.Items.AddChildObject(cxTreeView1.Items[J], ADemoUnitInfo.Caption,  ADemoUnitInfo);
      end;
  end;
  ARoot.CustomSort(@TutorialSortindProc, 1, True);
  ARoot.Expand(True);
  cxTreeView1.Select(ARoot.getFirstChild);
end;

function TfrmMain.GetActiveFrameID: Integer;
begin
  if DemoUnit <> nil then
    Result := DemoUnit.ID
  else
    Result := -MaxInt;
end;

function TfrmMain.FindDemoByID(ID: Integer; out ADemo: TDemoUnitInfo): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to FDemoUnits.Count - 1 do
  begin
    ADemo := TDemoUnitInfo(FDemoUnits[I]);
    if ID = ADemo.ID then
      Exit;
  end;
  Result := False;
end;

procedure TfrmMain.SetActiveFrameID(const AValue: Integer);
var
  ADemo: TDemoUnitInfo;
begin
  if (ActiveFrameID <> AValue) and FindDemoByID(AValue, ADemo) then
    DemoUnit := ADemo;
end;

procedure TfrmMain.SetDemoUnit(const AValue: TDemoUnitInfo);
var
  APrevUnit: TDemoUnitInfo;
  ANeedSplash: Boolean;
begin
  if DemoUnit = AValue then Exit;
  APrevUnit := FDemoUnit;
  FDemoUnit := AValue;
  ANeedSplash := DemoUnit <> nil;
  if ANeedSplash then
    dxSetSplashVisibility(True, DemoUnit.Caption);
  if APrevUnit <> nil then
    APrevUnit.SetParent(nil);
  if DemoUnit <> nil then
    DemoUnit.SetParent(pnlDemoSite);
  Caption := GetMainFormCaption + ' - ' + GetDemoCaption;
  if ANeedSplash then
    dxSetSplashVisibility(False);
  CheckShowCode;
end;

procedure TfrmMain.tsShowCodePropertiesEditValueChanged(Sender: TObject);
begin
  CheckShowCode;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Caption := GetMainFormCaption;
  Application.Title := Caption;
  TdxToggleSwitchPropertiesAccess(tsShowCode.ActiveProperties).SmoothToggle := False;
  PopulateTreeView;
  cxTreeView1Click(Self);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  DemoUnit := nil;
end;

initialization
  FDemoUnits := TObjectList<TDemoUnitInfo>.Create;
  FDemoUnits.Add(TDemoUnitGroupInfo.Create('ExpressEntityMapping Framework', EMFTreeViewRootID, -1, 0)); // root node
  FDemoUnits.Add(TDemoUnitGroupInfo.Create('Creating Data', EMFTreeViewTutorialEntityCreationGroupID, EMFTreeViewRootID, 1));
  FDemoUnits.Add(TDemoUnitGroupInfo.Create('Deleting Data', EMFTreeViewTutorialEntityDeletingGroupID, EMFTreeViewRootID, 3));
  FDemoUnits.Add(TDemoUnitGroupInfo.Create('Updating Data', EMFTreeViewTutorialEntityModificationGroupID, EMFTreeViewRootID, 2));
  FDemoUnits.Add(TDemoUnitGroupInfo.Create('Querying Data', EMFTreeViewTutorialEntityQueryingGroupID, EMFTreeViewRootID, 4));

finalization
  FDemoUnits.Free;

end.

