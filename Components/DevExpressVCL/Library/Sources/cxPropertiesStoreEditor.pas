{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library classes                   }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
{   ACCOMPANYING VCL AND CLX CONTROLS AS PART OF AN EXECUTABLE       }
{   PROGRAM ONLY.                                                    }
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

unit cxPropertiesStoreEditor;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ToolWin, ExtCtrls, StdCtrls, cxPropertiesStore, cxStorage, ActnList,
  cxControls, ImgList, cxClasses, cxGraphics, cxLookAndFeelPainters, cxGeometry, dxForms;

type
  TfrmPropertiesStoreFilter = (psfNone, psfStored, psfUnStored);
  TfrmPropertiesStoreGrouping = (psgComponents, psgProperties);

  { TcxPropertiesStoreNode }

  TcxPropertiesStoreNode = class
  private
    FName: string;
    FParent: TcxPropertiesStoreNode;
    FPersistent: TPersistent;
    FPropertyName: string;
    FSubNodes: TcxObjectList;

    FTreeNode: TTreeNode;
    FCheckState: TcxCheckBoxState;

    function GetIsPartOfComponent: Boolean;
    function GetNode(Index: Integer): TcxPropertiesStoreNode;
    function GetNodesCount: Integer;

    function GetStateIndex: Integer;
    procedure SetCheckState(Value: TcxCheckBoxState);

    function GetIsFullStored: Boolean;
    function GetIsStored: Boolean;
  public
    destructor Destroy; override;
    function Add(const AName: string; APersistent: TPersistent): TcxPropertiesStoreNode; overload;
    function Add(const AName, APropertyName: string; APersistent: TPersistent): TcxPropertiesStoreNode; overload;
    function Find(const AName: string; out ANode: TcxPropertiesStoreNode): Boolean; overload;
    function Find(const AName: string; var AIndex: Integer): Boolean; overload;
    function FindPersistent(APersistent: TPersistent;
      out ANode: TcxPropertiesStoreNode; ACanBePartOfComponent: Boolean = True): Boolean;
    procedure Clear;
    //
    procedure CheckStoredValue;
    procedure SetCheckValue(AValue: TcxCheckBoxState; AWithChildren, AWithParents: Boolean);
    procedure SetStoredValue(AValue, AWithChildren, AWithParents: Boolean);
    //
    property IsPartOfComponent: Boolean read GetIsPartOfComponent;
    property Name: string read FName;
    property Nodes[Index: Integer]: TcxPropertiesStoreNode read GetNode; default;
    property NodesCount: Integer read GetNodesCount;
    property Parent: TcxPropertiesStoreNode read FParent;
    property Persistent: TPersistent read FPersistent;
    property PropertyName: string read FPropertyName;

    property IsFullStored: Boolean read GetIsFullStored;
    property IsStored: Boolean read GetIsStored;
  end;

  { TfrmPropertiesStoreEditor }

  TfrmPropertiesStoreEditor = class(TdxForm)
    pnlClient: TPanel;
    ToolBar: TToolBar;
    pnlLeftTree: TPanel;
    pnlLeftTreeTop: TPanel;
    Tree: TTreeView;
    lblFindComponent: TLabel;
    edFindComponent: TEdit;
    btnGroupByComponents: TToolButton;
    btnGroupByProperties: TToolButton;
    ToolButton3: TToolButton;
    btnReset: TToolButton;
    btnCheckAll: TToolButton;
    btnUncheckAll: TToolButton;
    ActionList1: TActionList;
    actGroupByComponents: TAction;
    actGroupByProperties: TAction;
    cxImageList1: TcxImageList;
    Panel3: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    cxImageList2: TcxImageList;
    procedure actGroupByComponentsExecute(Sender: TObject);
    procedure actGroupByPropertiesExecute(Sender: TObject);
    procedure btnCheckAllClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnUncheckAllClick(Sender: TObject);
    procedure edFindComponentKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TreeExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    procedure TreeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TreeKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TreeKeyPress(Sender: TObject; var Key: Char);
  private
    FFilter: TfrmPropertiesStoreFilter;
    FGrouping: TfrmPropertiesStoreGrouping;
    FOwnerComponent: TComponent;
    FPropertiesStore: TcxPropertiesStore;
    FStructure: TcxPropertiesStoreNode;
    procedure SetOwnerComponent(const Value: TComponent);
    procedure SetFilter(const Value: TfrmPropertiesStoreFilter);
    procedure SetGrouping(const Value: TfrmPropertiesStoreGrouping);
    procedure SetFindText;
    procedure LoadFromPropertiesStore(AStore: TcxPropertiesStore);
    procedure SaveToPropertiesStore(AStore: TcxPropertiesStore);
    procedure BeginUpdate;
    procedure EndUpdate;
    function FindNode(const AText: string): TTreeNode;
    procedure Reset;
    //
    procedure CheckUncheckAll(AChecked: Boolean);
    procedure ToggleCheckState(ATreeNode: TTreeNode);
  protected
    procedure RefreshTree;
    procedure SynchronizeTreeView;
    procedure SynchronizeTreeViewNode(ATreeNode: TTreeNode; ANode: TcxPropertiesStoreNode);
    //
    property Structure: TcxPropertiesStoreNode read FStructure;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //
    property Filter: TfrmPropertiesStoreFilter read FFilter write SetFilter;
    property Grouping: TfrmPropertiesStoreGrouping read FGrouping write SetGrouping;
    property OwnerComponent: TComponent read FOwnerComponent write SetOwnerComponent;
    property PropertiesStore: TcxPropertiesStore read FPropertiesStore write FPropertiesStore;
  end;

const
  scxFindComponent = 'Find Component:';
  scxFindProperty = 'Find Property:';

procedure ShowPropertiesStoreEditor(APropertiesStore: TcxPropertiesStore;
  AOwnerComponent: TComponent);

implementation

{$R *.dfm}

uses
  Types, TypInfo, dxCore, Math, dxDPIAwareUtils;

type

  { TcxPropertiesStoreStructBuilder }

  TcxPropertiesStoreStructBuilder = class
  private
    FStructure: TcxPropertiesStoreNode;
    procedure FreePropList(APropList: PPropList; APropCount: Integer);
  public
    procedure AddComponent(AComponent: TComponent; AGroupByComponents: Boolean);
    procedure AddComponentProperties(APersistent: TPersistent;
      APersistentNode: TcxPropertiesStoreNode; AGroupByComponents: Boolean;
      APropList: PPropList; APropCount: Integer);
    procedure AddPersistent(APersistent: TPersistent; const AName: string;
      AGroupByComponents: Boolean; AParentNode: TcxPropertiesStoreNode;
      APersistentObject: TPersistent = nil);
    //
    property Structure: TcxPropertiesStoreNode read FStructure;
  end;

  { TcxPropertiesStoreIO }

  TcxPropertiesStoreIO = class
  private
    FGroupByComponents: Boolean;
    FStructure: TcxPropertiesStoreNode;
    function GetPersistentNodeByProperty(var APropertyName: string;
      APersistent: TPersistent; out ANode: TcxPropertiesStoreNode): Boolean;
    function ExtractName(var AName: string): string;
    function GetPropertyNode(AParentNode: TcxPropertiesStoreNode;
      APropertyName: string; out ANode: TcxPropertiesStoreNode): Boolean;
  protected
    procedure LoadGroupByComponent(AStore: TcxPropertiesStore);
    procedure LoadGroupByProperties(AStore: TcxPropertiesStore);
    procedure SaveComponent(AStore: TcxPropertiesStore; ANode: TcxPropertiesStoreNode);
    procedure SaveComponentsByProperties(AStore: TcxPropertiesStore; ANode: TcxPropertiesStoreNode);
    procedure SaveComponentProperties(AStoreComponent: TcxPropertiesStoreComponent;
      ANode: TcxPropertiesStoreNode; const AName: string);
  public
    constructor Create(AStructure: TcxPropertiesStoreNode; AGroupByComponents: Boolean);
    procedure Load(AStore: TcxPropertiesStore);
    procedure Save(AStore: TcxPropertiesStore);
    //
    property GroupByComponents: Boolean read FGroupByComponents;
    property Structure: TcxPropertiesStoreNode read FStructure;
  end;

procedure ShowPropertiesStoreEditor(APropertiesStore: TcxPropertiesStore;
  AOwnerComponent: TComponent);
var
  AForm: TfrmPropertiesStoreEditor;
begin
  AForm := TfrmPropertiesStoreEditor.Create(nil);
  AForm.PopupMode := pmAuto;
  AForm.OwnerComponent := AOwnerComponent;
  AForm.PropertiesStore := APropertiesStore;
  try
    if AForm.ShowModal = mrOK then
    begin
      AForm.SaveToPropertiesStore(APropertiesStore);
      SetDesignerModified(APropertiesStore);
    end;
  finally
    AForm.Free;
  end;
end;

{ TcxPropertiesStoreIO }

constructor TcxPropertiesStoreIO.Create(
  AStructure: TcxPropertiesStoreNode; AGroupByComponents: Boolean);
begin
  inherited Create;
  FStructure := AStructure;
  FGroupByComponents := AGroupByComponents;
end;

function TcxPropertiesStoreIO.ExtractName(var AName: string): string;
var
  AIndex: Integer;
begin
  Result := '';
  AIndex := Pos('.', AName);
  if AIndex > 0 then
  begin
    if AIndex > 1 then
      Result := Copy(AName, 1, AIndex - 1);
    Delete(AName, 1, AIndex);
  end
  else
  begin
    Result := AName;
    AName := '';
  end;
end;

procedure TcxPropertiesStoreIO.Load(AStore: TcxPropertiesStore);
begin
  if AStore <> nil then
  begin
    Structure.SetStoredValue(False, True, True);
    if GroupByComponents then
      LoadGroupByComponent(AStore)
    else
      LoadGroupByProperties(AStore);
  end;
end;

procedure TcxPropertiesStoreIO.LoadGroupByComponent(AStore: TcxPropertiesStore);
var
  AComponent: TcxPropertiesStoreComponent;
  ANode: TcxPropertiesStoreNode;
  APropertyNode: TcxPropertiesStoreNode;
  I, J: Integer;
begin
  for I := 0 to AStore.Components.Count - 1 do
  begin
    AComponent := AStore.Components[I];
    if Structure.FindPersistent(AComponent.Component, ANode, False) then
    begin
      for J := 0 to AComponent.Properties.Count - 1 do
      begin
        if GetPropertyNode(ANode, AComponent.Properties[J], APropertyNode) then
          APropertyNode.SetStoredValue(True, True, True);
      end;
    end;
  end;
end;

procedure TcxPropertiesStoreIO.LoadGroupByProperties(AStore: TcxPropertiesStore);
var
  AComponent: TcxPropertiesStoreComponent;
  ANode: TcxPropertiesStoreNode;
  APropertyName: string;
  I, J: Integer;
begin
  for I := 0 to AStore.Components.Count - 1 do
  begin
    AComponent := AStore.Components[I];
    for J := 0 to AComponent.Properties.Count - 1 do
    begin
      APropertyName := AComponent.Properties[J];
      if GetPersistentNodeByProperty(APropertyName, AComponent.Component, ANode) then
      begin
        if APropertyName <> '' then
        begin
          if not GetPropertyNode(ANode, APropertyName, ANode) then
            ANode := nil;
        end;
        if ANode <> nil then
          ANode.SetStoredValue(True, True, True);
      end;
    end;
  end;
end;

procedure TcxPropertiesStoreIO.Save(AStore: TcxPropertiesStore);
var
  ANode: TcxPropertiesStoreNode;
  I: Integer;
begin
  if AStore <> nil then
  begin
    AStore.Components.BeginUpdate;
    try
      AStore.Components.Clear;
      for I := 0 to Structure.NodesCount - 1 do
      begin
        ANode := Structure.Nodes[I];
        if ANode.IsStored then
        begin
          if GroupByComponents then
            SaveComponent(AStore, ANode)
          else
            SaveComponentsByProperties(AStore, ANode);
        end;
      end;
    finally
      AStore.Components.EndUpdate;
    end;
  end;
end;

procedure TcxPropertiesStoreIO.SaveComponent(
  AStore: TcxPropertiesStore; ANode: TcxPropertiesStoreNode);
var
  AStoreComponent: TcxPropertiesStoreComponent;
begin
  if ANode.Persistent is TComponent then
  begin
    AStoreComponent := AStore.Components.Add;
    AStoreComponent.Component := TComponent(ANode.Persistent);
    SaveComponentProperties(AStoreComponent, ANode, '');
  end;
end;

procedure TcxPropertiesStoreIO.SaveComponentsByProperties(
  AStore: TcxPropertiesStore; ANode: TcxPropertiesStoreNode);

  function GetPropertiesStoreComponent(AComponent: TComponent): TcxPropertiesStoreComponent;
  begin
    if not AStore.Components.FindComponentItemByComponent(AComponent, Result) then
    begin
      Result := AStore.Components.Add;
      Result.Component := AComponent;
    end;
  end;

var
  AStoreComponent: TcxPropertiesStoreComponent;
  ASubNode: TcxPropertiesStoreNode;
  I: Integer;
begin
  for I := 0 to ANode.NodesCount - 1 do
  begin
    ASubNode := ANode[I];
    if ASubNode.IsStored then
    begin
      if ASubNode.Persistent is TComponent then
      begin
         AStoreComponent := GetPropertiesStoreComponent(TComponent(ASubNode.Persistent));
         if ASubNode.IsFullStored then
           AStoreComponent.Properties.Add(ANode.Name)
         else
           SaveComponentProperties(AStoreComponent, ASubNode, ANode.Name + '.');
       end;
    end;
  end;
end;

procedure TcxPropertiesStoreIO.SaveComponentProperties(
  AStoreComponent: TcxPropertiesStoreComponent;
  ANode: TcxPropertiesStoreNode; const AName: string);
var
  ASubNode: TcxPropertiesStoreNode;
  I: Integer;
begin
  for I := 0 to ANode.NodesCount - 1 do
  begin
    ASubNode := ANode[I];
    if ASubNode.IsFullStored then
      AStoreComponent.Properties.Add(AName + ASubNode.Name)
    else
      if ASubNode.IsStored then
        SaveComponentProperties(AStoreComponent, ASubNode, AName + ASubNode.Name + '.');
  end;
end;

function TcxPropertiesStoreIO.GetPropertyNode(AParentNode: TcxPropertiesStoreNode;
  APropertyName: string; out ANode: TcxPropertiesStoreNode): Boolean;
var
  ACurrentName: string;
begin
  Result := False;
  ACurrentName := ExtractName(APropertyName);
  if ACurrentName <> '' then
  begin
    if AParentNode.Find(ACurrentName, ANode) then
    begin
      if APropertyName <> '' then
        Result := GetPropertyNode(ANode, APropertyName, ANode)
      else
        Result := True;
    end;
  end;
end;

function TcxPropertiesStoreIO.GetPersistentNodeByProperty(var APropertyName: string;
  APersistent: TPersistent; out ANode: TcxPropertiesStoreNode): Boolean;
var
  ACurrentName: string;
  APropertyNode: TcxPropertiesStoreNode;
begin
  Result := False;
  if (APropertyName <> '') and (APersistent <> nil) then
  begin
    ACurrentName := ExtractName(APropertyName);
    if ACurrentName <> '' then
    begin
      if Structure.Find(ACurrentName, APropertyNode) then
        Result := APropertyNode.FindPersistent(APersistent, ANode);
    end;
  end;
end;

{ TcxPropertiesStoreStructBuilder }

procedure TcxPropertiesStoreStructBuilder.AddComponent(
  AComponent: TComponent; AGroupByComponents: Boolean);
begin
  AddPersistent(AComponent, AComponent.Name, AGroupByComponents, Structure);
end;

procedure TcxPropertiesStoreStructBuilder.AddComponentProperties(
  APersistent: TPersistent; APersistentNode: TcxPropertiesStoreNode;
  AGroupByComponents: Boolean; APropList: PPropList; APropCount: Integer);
var
  AObject: TObject;
  APropInfo: PPropInfo;
  I: Integer;
begin
  for I := 0 to APropCount - 1 do
  begin
    APropInfo := APropList[I];
    if APropInfo.PropType^.Kind <> tkMethod then
    begin
      if APropInfo.PropType^.Kind = tkClass then
      begin
        AObject := GetObjectProp(APersistent, APropInfo);
        if (AObject is TPersistent) and not (AObject is TComponent) then
        begin
          AddPersistent(TPersistent(AObject),
            dxShortStringToString(APropInfo.Name),
            AGroupByComponents, APersistentNode, APersistent);
          Continue;
        end;
      end;
      APersistentNode.Add(dxShortStringToString(APropInfo.Name), APersistent)
    end
  end;
end;

procedure TcxPropertiesStoreStructBuilder.AddPersistent(
  APersistent: TPersistent; const AName: string; AGroupByComponents: Boolean;
  AParentNode: TcxPropertiesStoreNode; APersistentObject: TPersistent = nil);
var
  ANode: TcxPropertiesStoreNode;
  AObject: TObject;
  APropCount, I: Integer;
  APropInfo: PPropInfo;
  APropList: PPropList;
begin
  APropCount := GetPropList(APersistent, APropList);
  try
    if AGroupByComponents then
    begin
      if APersistentObject = nil then
        APersistentObject := APersistent;
      AddComponentProperties(APersistent,
        AParentNode.Add(AName, '', APersistentObject),
        AGroupByComponents, APropList, APropCount);
    end
    else
      for I := 0 to APropCount - 1 do
      begin
        APropInfo := APropList[I];
        if APropInfo.PropType^.Kind <> tkMethod then
        begin
          ANode := Structure.Add(dxShortStringToString(APropInfo.Name), '', nil);
          if APropInfo.PropType^.Kind = tkClass then
          begin
            AObject := GetObjectProp(APersistent, APropInfo);
            if (AObject is TPersistent) and not (AObject is TComponent) then
            begin
              AddPersistent(TPersistent(AObject), AName, True, ANode, APersistent);
              Continue;
            end;
          end;
          ANode.Add(AName, dxShortStringToString(APropInfo.Name), APersistent);
        end;
      end;
  finally
    FreePropList(APropList, APropCount);
  end;
end;

procedure TcxPropertiesStoreStructBuilder.FreePropList(APropList: PPropList; APropCount: Integer);
begin
  if APropCount > 0 then
    FreeMem(APropList, APropCount * SizeOf(Pointer));
end;

{ TcxPropertiesStoreNode }

destructor TcxPropertiesStoreNode.Destroy;
begin
  Clear;
  FreeAndNil(FSubNodes);
  inherited Destroy;
end;

function TcxPropertiesStoreNode.Add(const AName: string; APersistent: TPersistent): TcxPropertiesStoreNode;
begin
  Result := Add(AName, AName, APersistent);
end;

function TcxPropertiesStoreNode.Add(const AName, APropertyName: string; APersistent: TPersistent): TcxPropertiesStoreNode;
var
  AIndex: Integer;
begin
  if Find(AName, AIndex) then
    Result := Nodes[AIndex]
  else
  begin
    Result := TcxPropertiesStoreNode.Create;
    Result.FName := AName;
    Result.FParent := Self;
    Result.FPropertyName := APropertyName;
    Result.FPersistent := APersistent;
    if FSubNodes = nil then
      FSubNodes := TcxObjectList.Create;
    FSubNodes.Insert(AIndex, Result);
  end;
end;

function TcxPropertiesStoreNode.Find(const AName: string; var AIndex: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := NodesCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareText(Nodes[I].Name, AName);
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  AIndex := L;
end;

function TcxPropertiesStoreNode.FindPersistent(APersistent: TPersistent;
  out ANode: TcxPropertiesStoreNode; ACanBePartOfComponent: Boolean = True): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to NodesCount - 1 do
  begin
    Result := (Nodes[I].Persistent = APersistent) and (ACanBePartOfComponent or not Nodes[I].IsPartOfComponent);
    if Result then
    begin
      ANode := Nodes[I];
      Break;
    end;
  end;
end;

function TcxPropertiesStoreNode.Find(const AName: string; out ANode: TcxPropertiesStoreNode): Boolean;
var
  AIndex: Integer;
begin
  Result := Find(AName, AIndex);
  if Result then
    ANode := Nodes[AIndex];
end;

function TcxPropertiesStoreNode.GetIsFullStored: Boolean;
var
  I: Integer;
begin
  Result := IsStored;
  for I := 0 to NodesCount - 1 do
    Result := Result and Nodes[I].IsFullStored;
end;

function TcxPropertiesStoreNode.GetIsPartOfComponent: Boolean;
begin
  Result := PropertyName <> '';
end;

function TcxPropertiesStoreNode.GetNode(Index: Integer): TcxPropertiesStoreNode;
begin
  if FSubNodes <> nil then
    Result := TcxPropertiesStoreNode(FSubNodes[Index])
  else
    Result := nil;
end;

function TcxPropertiesStoreNode.GetNodesCount: Integer;
begin
  if FSubNodes <> nil then
    Result := FSubNodes.Count
  else
    Result := 0;
end;

function TcxPropertiesStoreNode.GetStateIndex: Integer;
const
  StateIndexesMap: array[TcxCheckBoxState] of Integer = (1, 2, 3);
begin
  Result := StateIndexesMap[FCheckState]
end;

procedure TcxPropertiesStoreNode.SetCheckState(Value: TcxCheckBoxState);
begin
  if FCheckState <> Value then
  begin
    FCheckState := Value;
    if FTreeNode <> nil then
      FTreeNode.StateIndex := GetStateIndex;
  end;
end;

function TcxPropertiesStoreNode.GetIsStored: Boolean;
begin
  Result := FCheckState <> cbsUnchecked;
end;

procedure TcxPropertiesStoreNode.Clear;
begin
  FreeAndNil(FSubNodes);
end;

procedure TcxPropertiesStoreNode.CheckStoredValue;
var
  I: Integer;
  AChildState: TcxCheckBoxState;
begin
  if NodesCount > 0 then
  begin
    AChildState := Nodes[0].FCheckState;
    for I := 0 to NodesCount - 1 do
      if (AChildState = cbsGrayed) or (AChildState <> Nodes[I].FCheckState) then
      begin
        AChildState := cbsGrayed;
        Break;
      end;
    SetCheckValue(AChildState, False, True);
  end;
end;

procedure TcxPropertiesStoreNode.SetCheckValue(AValue: TcxCheckBoxState; AWithChildren, AWithParents: Boolean);
var
  I: Integer;
begin
  if FCheckState <> AValue then
  begin
    SetCheckState(AValue);

    if AWithChildren then
    begin
      for I := 0 to NodesCount - 1 do
        Nodes[I].SetCheckValue(AValue, True, False);
    end;

    if AWithParents then
    begin
      if Parent <> nil then
        Parent.CheckStoredValue;
    end;
  end;
end;

procedure TcxPropertiesStoreNode.SetStoredValue(AValue, AWithChildren, AWithParents: Boolean);
var
  ACheckState: TcxCheckBoxState;
begin
  if AValue then
    ACheckState := cbsChecked
  else
    ACheckState := cbsUnchecked;
  SetCheckValue(ACheckState, AWithChildren, AWithParents);
end;

{ TfrmPropertiesStoreEditor }

constructor TfrmPropertiesStoreEditor.Create(AOwner: TComponent);

  procedure AddCheckImage(ACheckState: TcxCheckBoxState);
  var
    ACheck: TcxBitmap;
  begin
    ACheck := TcxBitmap.CreateSize(cxImageList1.Width, cxImageList1.Height, pf32bit);
    try
      if IsXPManifestEnabled then
        ACheck.cxCanvas.FillRect(ACheck.ClientRect, clBlack)
      else
        ACheck.cxCanvas.FillRect(ACheck.ClientRect, clWindow);

      cxLookAndFeelPaintersManager.GetPainter(lfsNative).DrawScaledCheckButton(ACheck.cxCanvas,
        cxRectCenter(ACheck.ClientRect, ACheck.Width - 3, ACheck.Height - 3), cxbsNormal, ACheckState, ScaleFactor);
      cxImageList1.Add(ACheck, nil);
    finally
      ACheck.Free;
    end;
  end;

begin
  inherited Create(AOwner);
  FStructure := TcxPropertiesStoreNode.Create;

  if not IsXPManifestEnabled then
    cxTransformImages(cxImageList2, clBtnFace);

  Tree.StateImages := cxImageList1;
  AddCheckImage(cbsUnchecked);
  AddCheckImage(cbsUnchecked);
  AddCheckImage(cbsChecked);
  AddCheckImage(cbsGrayed);
end;

destructor TfrmPropertiesStoreEditor.Destroy;
begin
  FreeAndNil(FStructure);
  inherited Destroy;
end;

procedure TfrmPropertiesStoreEditor.RefreshTree;
var
  ABuilder: TcxPropertiesStoreStructBuilder;
  I: Integer;
begin
  Structure.Clear;
  if OwnerComponent <> nil then
  begin
    ABuilder := TcxPropertiesStoreStructBuilder.Create;
    try
      ABuilder.FStructure := Structure;
      ABuilder.AddComponent(OwnerComponent, Grouping = psgComponents);
      for I := 0 to OwnerComponent.ComponentCount - 1 do
        ABuilder.AddComponent(OwnerComponent.Components[I], Grouping = psgComponents);
    finally
      ABuilder.Free;
    end;
  end;
  SynchronizeTreeView;
end;

procedure TfrmPropertiesStoreEditor.SynchronizeTreeView;
var
  I: Integer;
begin
  Tree.Items.BeginUpdate;
  try
    Tree.Items.Clear;
    for I := 0 to Structure.NodesCount - 1 do
      SynchronizeTreeViewNode(Tree.Items.AddChild(nil, ''), Structure.Nodes[I]);
  finally
    Tree.Items.EndUpdate;
  end;
end;

procedure TfrmPropertiesStoreEditor.SynchronizeTreeViewNode(
  ATreeNode: TTreeNode; ANode: TcxPropertiesStoreNode);
begin
  ATreeNode.Text := ANode.Name;
  ATreeNode.Data := ANode;
  ATreeNode.HasChildren := ANode.NodesCount > 0;
  ATreeNode.StateIndex := ANode.GetStateIndex;
  ANode.FTreeNode := ATreeNode;
end;

procedure TfrmPropertiesStoreEditor.SetFilter(const Value: TfrmPropertiesStoreFilter);
begin
  if Filter <> Value then
  begin
    FFilter := Value;
    RefreshTree;
  end;
end;

procedure TfrmPropertiesStoreEditor.SetGrouping(const Value: TfrmPropertiesStoreGrouping);
var
  APropertiesStore: TcxPropertiesStore;
  ALastValue: TfrmPropertiesStoreGrouping;
begin
  if Grouping <> Value then
  begin
    APropertiesStore := TcxPropertiesStore.Create(nil);
    try
      SaveToPropertiesStore(APropertiesStore);
      ALastValue := FGrouping;
      FGrouping := Value;
      try
        RefreshTree;
        LoadFromPropertiesStore(APropertiesStore);
        SetFindText;
      except
        FGrouping := ALastValue;
        raise;
      end;
    finally
      APropertiesStore.Free;
    end;
  end;
end;

procedure TfrmPropertiesStoreEditor.ToggleCheckState(ATreeNode: TTreeNode);
var
  ANode: TcxPropertiesStoreNode;
begin
  if (ATreeNode <> nil) and (ATreeNode.Data <> nil) then
  begin
    BeginUpdate;
    try
      ANode := TcxPropertiesStoreNode(ATreeNode.Data);
      ANode.SetStoredValue(not ANode.IsStored, True, True);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TfrmPropertiesStoreEditor.SetOwnerComponent(const Value: TComponent);
begin
  if FOwnerComponent <> Value then
  begin
    BeginUpdate;
    try
      FOwnerComponent := Value;
      RefreshTree;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TfrmPropertiesStoreEditor.TreeExpanding(
  Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
var
  AStoreNode: TcxPropertiesStoreNode;
  I: Integer;
begin
  BeginUpdate;
  try
    Node.DeleteChildren;
    AStoreNode := TcxPropertiesStoreNode(Node.Data);
    for I := 0 to AStoreNode.NodesCount - 1 do
      SynchronizeTreeViewNode(Tree.Items.AddChild(Node, ''), AStoreNode.Nodes[I]);
  finally
    EndUpdate;
  end;
end;

procedure TfrmPropertiesStoreEditor.FormCreate(Sender: TObject);
begin
  FGrouping := psgComponents;
end;

procedure TfrmPropertiesStoreEditor.actGroupByComponentsExecute(Sender: TObject);
begin
  BeginUpdate;
  try
    Grouping := psgComponents;
  finally
    EndUpdate;
  end;
end;

procedure TfrmPropertiesStoreEditor.actGroupByPropertiesExecute(
  Sender: TObject);
begin
  BeginUpdate;
  try
    Grouping := psgProperties;
  finally
    EndUpdate;
  end;
end;

procedure TfrmPropertiesStoreEditor.btnCheckAllClick(Sender: TObject);
begin
  CheckUncheckAll(True);
end;

procedure TfrmPropertiesStoreEditor.btnUncheckAllClick(Sender: TObject);
begin
  CheckUncheckAll(False);
end;

procedure TfrmPropertiesStoreEditor.FormShow(Sender: TObject);
begin
  LoadFromPropertiesStore(PropertiesStore);
end;

procedure TfrmPropertiesStoreEditor.btnResetClick(Sender: TObject);
begin
  Reset;
end;

procedure TfrmPropertiesStoreEditor.BeginUpdate;
begin
  Tree.Items.BeginUpdate;
end;

procedure TfrmPropertiesStoreEditor.EndUpdate;
begin
  Tree.Items.EndUpdate;
end;

procedure TfrmPropertiesStoreEditor.SetFindText;
const
  AGroupingCaptionMap: array [TfrmPropertiesStoreGrouping] of string = (scxFindComponent, scxFindProperty);
begin
  lblFindComponent.Caption := AGroupingCaptionMap[Grouping];
end;

procedure TfrmPropertiesStoreEditor.edFindComponentKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
var
  ANode: TTreeNode;
begin
  if Key = VK_RETURN then
  begin
    ANode := FindNode(edFindComponent.Text);
    if ANode <> nil then
    begin
      Tree.Selected := ANode;
      Tree.SetFocus;
    end;
  end;
end;

function TfrmPropertiesStoreEditor.FindNode(const AText: string): TTreeNode;

  function PartialCompare(const AUpperCaseText: string; ANode: TTreeNode): Integer;
  var
    ANodeUpperCaseText: string;
    I: Integer;
  begin
    Result := 0;
    ANodeUpperCaseText := UpperCase(ANode.Text);
    for I := 1 to Min(Length(AUpperCaseText), Length(ANodeUpperCaseText)) do
    begin
      if AUpperCaseText[I] = ANodeUpperCaseText[I] then
        Inc(Result)
      else
        Break;
    end;
  end;

var
  ACompareResult: Integer;
  AMaxCompareResult: Integer;
  ANode: TTreeNode;
  AUpperCaseText: string;
begin
  Result := nil;
  AMaxCompareResult := 0;
  AUpperCaseText := UpperCase(AText);
  ANode := Tree.Items.GetFirstNode;
  while ANode <> nil do
  begin
    ACompareResult := PartialCompare(AUpperCaseText, ANode);
    if ACompareResult > AMaxCompareResult then
    begin
      AMaxCompareResult := ACompareResult;
      Result := ANode;
    end;
    ANode := ANode.getNextSibling;
  end;
end;

procedure TfrmPropertiesStoreEditor.CheckUncheckAll(AChecked: Boolean);
begin
  BeginUpdate;
  try
    Structure.SetStoredValue(AChecked, True, True);
  finally
    EndUpdate;
  end;
end;

procedure TfrmPropertiesStoreEditor.Reset;
begin
  BeginUpdate;
  try
    RefreshTree;
    LoadFromPropertiesStore(PropertiesStore);
  finally
    EndUpdate;
  end;
end;

procedure TfrmPropertiesStoreEditor.LoadFromPropertiesStore(AStore: TcxPropertiesStore);
begin
  with TcxPropertiesStoreIO.Create(Structure, Grouping = psgComponents) do
  try
    Load(AStore);
  finally
    Free;
  end;
end;

procedure TfrmPropertiesStoreEditor.SaveToPropertiesStore(AStore: TcxPropertiesStore);
begin
  with TcxPropertiesStoreIO.Create(Structure, Grouping = psgComponents) do
  try
    Save(AStore);
  finally
    Free;
  end;
end;

procedure TfrmPropertiesStoreEditor.TreeMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ANode: TTreeNode;
begin
  ANode := Tree.GetNodeAt(X, Y);
  if (ANode <> nil) and (htOnStateIcon in Tree.GetHitTestInfoAt(X, Y)) then
    ToggleCheckState(ANode);
end;

procedure TfrmPropertiesStoreEditor.TreeKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_SPACE then
    ToggleCheckState(Tree.Selected);
end;

procedure TfrmPropertiesStoreEditor.TreeKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = ' ' then
    Key := #0;
end;

end.
