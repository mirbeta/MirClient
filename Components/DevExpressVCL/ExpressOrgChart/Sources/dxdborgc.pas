{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express data-aware OrgChart                              }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSORGCHART AND ALL ACCOMPANYING  }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE end USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxdborgc;

{$I cxVer.inc}

interface

uses
  Types, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, dxorgchr, Variants;

type

  TdxDbOrgChart = class;
  TdxOcNewKeyEvent = procedure(Sender:TObject; MaxValue:Variant; var KeyValue:Variant) of object;

  TdxOcDataLink = class(TDataLink)
  private
    FTree: TdxDbOrgChart;
    FFiltered: Boolean;
    FFilter: String;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure RecordChanged(Field: TField); override;
  end;

  TdxDbOcNode = class(TdxOcNode)
  private
    FKey: Variant;
    FParentKey: Variant;
    FOrder: Variant;
    FRecNum: Integer;
    FRefreshed: Boolean;
  protected
    procedure SetText(const Value: String); override;
    procedure ReadData(Stream: TStream); override;
  public
    constructor Create(AOwner: TdxCustomOrgChart); override;
    function DbOwner: TdxDbOrgChart;
    property Key: Variant read FKey;
    property ParentKey: Variant read FParentKey;
  end;

  TdxDbOrgChart = class(TdxCustomOrgChart)
  private
    FLink: TdxOcDataLink;
    FKeyFieldName: String;
    FParentFieldName: String;
    FTextFieldName: String;
    FOrderFieldName: String;
    FWidthFieldName: String;
    FHeightFieldName: String;
    FColorFieldName: String;
    FShapeFieldName: String;
    FChAlignFieldName: String;
    FImIndexFieldName: String;
    FImAlignFieldName: String;
    FKeyField: TField;
    FParentField: TField;
    FTextField: TField;
    FOrderField: TField;
    FWidthField: TField;
    FHeightField: TField;
    FColorField: TField;
    FShapeField: TField;
    FChAlignField: TField;
    FImIndexField: TField;
    FImAlignField: TField;
    FKeyList: TList;
    FMaxKey: Variant;
    FCurRec: Integer;
    FRecCount: Integer;
    FOnNewKey: TdxOcNewKeyEvent;
    FOnLoadNode: TdxOcEvent;
    FEnableDB: Boolean;
    FKeyOrder: Boolean;
    procedure AssignFields;
    procedure RefreshItems;
    procedure RefreshRecord;
    procedure RefreshParents;
    procedure BeginRefresh;
    procedure EndRefresh;
    function FindNearest(AKey: Variant; var AIndex: Integer): Boolean;
    function GetNodeByKey(AKey: Variant): TdxDbOcNode;
    procedure AddToKeyList(ANode: TdxDbOcNode);
    procedure DelFromKeyList(ANode: TdxDbOcNode);
    function FindIndex(AKey: Variant; AParent: TdxDbOcNode): Integer;
    function LocateToNode(ANode: TdxDbOcNode): Boolean;
    procedure SetParentNode(ANode: TdxDbOcNode);
    procedure CheckKeys(ANode: TdxDbOcNode);
    procedure CheckRec(ANode: TdxDbOcNode);
    function NewKey: Variant;
    function CreateDBNode(AKey: Variant; AParent: TdxDbOcNode): TdxDbOcNode;
    procedure MoveDBNode(ANode,AParent: TdxDbOcNode);
    function GetDataSource: TDataSource;
    function GetDataSet: TDataSet;
    procedure ActiveChanged;
    procedure DataChanged;
    procedure DoScroll(Dist: Integer);
    procedure RecordChanged(Field: TField);
    procedure SetBookmark(ABookmark: Integer);
    procedure SetDataSource(Value: TDataSource);
    procedure SetKeyFieldName(const Value: String);
    procedure SetParentFieldName(const Value: String);
    procedure SetTextFieldName(const Value: String);
    procedure SetOrderFieldName(const Value: String);
    procedure SetWidthFieldName(const Value: String);
    procedure SetHeightFieldName(const Value: String);
    procedure SetColorFieldName(const Value: String);
    procedure SetShapeFieldName(const Value: String);
    procedure SetChAlignFieldName(const Value: String);
    procedure SetImIndexFieldName(const Value: String);
    procedure SetImAlignFieldName(const Value: String);
  protected
    function CreateEditor: TdxOcInplaceEdit; override;
    function InternalAdd(AParent: TdxOcNode; Data: TdxOcNodeData; Idx:Integer): TdxOcNode; override;
    procedure InternalMoveTo(AParent, ANode: TdxOcNode; Idx: Integer); override;
    procedure NodeChanged(ANode: TdxOcNode); override;
    procedure DoChange(Node: TdxOcNode); override;
    procedure DoChanging(Node: TdxOcNode; var Allow: Boolean); override;
    function GetNodeClass: TdxOcNodeClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Delete(ANode: TdxOcNode); override;
    procedure ShowEditor; override;
    function Active: Boolean;
    property DataSet: TDataSet read GetDataSet;
    property KeyField: TField read FKeyField;
    property ParentField: TField read FParentField;
    property TextField: TField read FTextField;
    property OrderField: TField read FOrderField;
    property WidthField: TField read FWidthField;
    property HeightField: TField read FHeightField;
    property ColorField: TField read FColorField;
    property ShapeField: TField read FShapeField;
    property ChAlignField: TField read FChAlignField;
    property ImageField: TField read FImIndexField;
    property ImAlignField: TField read FImAlignField;
    property WidthFieldName: String read FWidthFieldName write SetWidthFieldName;
    property HeightFieldName: String read FHeightFieldName write SetHeightFieldName;
    property ColorFieldName: String read FColorFieldName write SetColorFieldName;
    property ShapeFieldName: String read FShapeFieldName write SetShapeFieldName;
    property ChAlignFieldName: String read FChAlignFieldName write SetChAlignFieldName;
    property ImAlignFieldName: String read FImAlignFieldName write SetImAlignFieldName;
  published
    property Anchors;
    property Antialiasing;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property KeyFieldName: String read FKeyFieldName write SetKeyFieldName;
    property ParentFieldName: String read FParentFieldName write SetParentFieldName;
    property TextFieldName: String read FTextFieldName write SetTextFieldName;
    property OrderFieldName: String read FOrderFieldName write SetOrderFieldName;
    property ImageFieldName: String read FImIndexFieldName write SetImIndexFieldName;
    property KeyOrder: Boolean read FKeyOrder write FKeyOrder default False;
    property OnNewKey: TdxOcNewKeyEvent read FOnNewKey write FOnNewKey;
    property OnLoadNode: TdxOcEvent read FOnLoadNode write FOnLoadNode;
    property LookAndFeel;
    property LineColor;
    property LineWidth;
    property SelectedNodeColor;
    property SelectedNodeTextColor;
    property DefaultNodeWidth;
    property DefaultNodeHeight;
    property IndentX;
    property IndentY;
    property Options;
    property EditMode;
    property Images;
    property DefaultImageAlign;
    property BorderStyle;
    property Rotated;
    property Zoom;
    property OnCreateNode;
    property OnChange;
    property OnChanging;
    property OnCollapsed;
    property OnCollapsing;
    property OnDeletion;
    property OnExpanded;
    property OnExpansion;
    property OnEditing;
    property OnEdited;
    property OnSetFont;
    property OnDrawNode;
    property OnGetText;
    property OnSetText;
    property Align;
    property Ctl3D;
    property Color;
    property Enabled;
    property Font;
    property ParentColor default False;
    property ParentCtl3D;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnDragDrop;
    property OnDragOver;
    property OnStartDrag;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property PopupMenu;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
  end;

implementation

function VarCompare(const V1, V2: Variant): Integer;
begin
  try
    if V1 = V2 then
      Result := 0
    else
      if VarIsNull(V1) then
        Result := -1
      else
        if VarIsNull(V2) then
          Result := 1
        else
          if V1 < V2 then
            Result := -1
          else
            Result := 1;
  except
    on EVariantError do
      Result := -1;
  end
end;

function VarEQ(const V1,V2: Variant): Boolean;
begin
  try
    Result := V1 = V2;
  except
    Result := False;
  end;
end;

function VarGT(const V1,V2: Variant): Boolean;
begin
  try
    Result := VarCompare(V1, V2) > 0;
  except
    Result := False;
  end;
end;

function VarGE(const V1,V2: Variant): Boolean;
begin
  try
    Result := VarCompare(V1, V2) >= 0;
  except
    Result := False;
  end;
end;

function IsIntegerField(AField: TField): Boolean;
begin
  Result := (AField <> nil) and (AField.DataType in
    [ftInteger, ftSmallint, ftWord, ftAutoInc, ftLargeint, ftLongWord, ftShortint, ftByte]);
end;

{ TdxOcDataLink }

procedure TdxOcDataLink.ActiveChanged;
begin
  FTree.ActiveChanged;
end;

procedure TdxOcDataLink.DataSetChanged;
begin
  if (FFiltered<>DataSet.Filtered) or (FFilter<>DataSet.Filter)
  then FTree.ActiveChanged else FTree.DataChanged;
end;

procedure TdxOcDataLink.DataSetScrolled(Distance: Integer);
begin
  FTree.DoScroll(Distance);
end;

procedure TdxOcDataLink.RecordChanged(Field: TField);
begin
  FTree.RecordChanged(Field);
end;

{ TdxDbOcNode }

constructor TdxDbOcNode.Create(AOwner: TdxCustomOrgChart);
begin
  inherited;
  FKey := Null;
  FParentKey := Null;
end;

function TdxDbOcNode.DbOwner: TdxDbOrgChart;
begin
  Result := TdxDbOrgChart(Owner);
end;

procedure TdxDbOcNode.SetText(const Value: String);
var TheText: String;
begin
  TheText := Value;
  if (DbOwner.TextField<>nil) and (Length(TheText) > DbOwner.TextField.Size)
  then SetLength (TheText, DbOwner.TextField.Size);
  inherited SetText(TheText);
end;

procedure TdxDbOcNode.ReadData(Stream: TStream);
begin
  DbOwner.FEnableDB := False;
  inherited ReadData(Stream);
  DbOwner.FEnableDB := True;
  DbOwner.NodeChanged(Self);
end;

{ TdxDbOrgChart }

constructor TdxDbOrgChart.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLink := TdxOcDataLink.Create;
  FLink.FTree := Self;
  FKeyList := TList.Create;
  FMaxKey := null;
  FEnableDB := True;
end;

destructor TdxDbOrgChart.Destroy;
begin
  FEnableDB := False;
  FLink.Free;
  FKeyList.Free;
  FKeyList := nil;
  inherited Destroy;
end;

function TdxDbOrgChart.Active: Boolean;
begin
  Result := FLink.Active and (KeyField<>nil) and (ParentField<>nil);
end;

function TdxDbOrgChart.GetDataSet: TDataSet;
begin
  Result := FLink.DataSet;
end;

function TdxDbOrgChart.GetDataSource: TDataSource;
begin
  Result := FLink.DataSource;
end;

procedure TdxDbOrgChart.SetBookmark(ABookmark: Integer);
begin
  if ABookmark<>FCurRec
  then FCurRec := FCurRec + DataSet.MoveBy(ABookmark-FCurRec);
end;

procedure TdxDbOrgChart.ShowEditor;
begin
  if Active and DataSet.CanModify then inherited ShowEditor;
end;

procedure TdxDbOrgChart.AssignFields;

  procedure AssignField(var Field: TField; const FName: String);
  begin
    if FName <> '' then Field := DataSet.FindField(FName);
  end;

  procedure ChkInt(var Field: TField);
  begin
    if not IsIntegerField(Field) then Field := nil;
  end;

begin
  if FLink.Active then begin
    AssignField(FKeyField,FKeyFieldName);
    AssignField(FParentField,FParentFieldName);
    AssignField(FTextField,FTextFieldName);
    AssignField(FOrderField,FOrderFieldName);
    AssignField(FWidthField,FWidthFieldName);
    AssignField(FHeightField,FHeightFieldName);
    AssignField(FColorField,FColorFieldName);
    AssignField(FShapeField,FShapeFieldName);
    AssignField(FChAlignField,FChAlignFieldName);
    AssignField(FImIndexField,FImIndexFieldName);
    AssignField(FImAlignField,FImAlignFieldName);
    if (FOrderField <> nil) and (FOrderField.DataType <> ftFloat) then
      ChkInt(FOrderField);
    ChkInt(FWidthField);
    ChkInt(FHeightField);
    ChkInt(FColorField);
    ChkInt(FShapeField);
    ChkInt(FChAlignField);
    ChkInt(FImIndexField);
    ChkInt(FImAlignField);
  end;
end;

procedure TdxDbOrgChart.RefreshItems;
var
  Bm: Integer;
  CurKey: Variant;
begin
  if not Active then Exit;
  BeginRefresh;
  FEnableDB := False;
  DataSet.DisableControls;
  CurKey := KeyField.Value;
  FRecCount := -1; FCurRec := 0;
  FMaxKey := null;
  Bm := -DataSet.MoveBy(-99999999);
  while not DataSet.EOF do begin
    Inc(FRecCount);
    FCurRec := FRecCount;
    RefreshRecord;
    DataSet.Next;
  end;
  RefreshParents;
  EndRefresh;
  SetBookmark(Bm);
  DataSet.EnableControls;
  FEnableDB := True;
  if csDesigning in ComponentState then FullExpand;
end;

procedure TdxDbOrgChart.BeginRefresh;
var
  I: Integer;
begin
  for I := 0 to FKeyList.Count-1 do
    TdxDbOcNode(FKeyList[I]).FRefreshed := False;
end;

procedure TdxDbOrgChart.EndRefresh;
var
  I: Integer;
begin
  I := 0;
  while I < FKeyList.Count do
  begin
    if TdxDbOcNode(FKeyList[I]).FRefreshed then
      Inc(I)
    else
    begin
      Delete(TdxDbOcNode(FKeyList[I]));
      I := 0;
    end;
  end;
end;

procedure TdxDbOrgChart.RefreshRecord;

  function GetIntField(Field: TField; Default: Integer): Integer;
  begin
    if Field.IsNull then Result := Default
    else Result := Field.AsInteger;
  end;

var
  Node,Par: TdxDbOcNode;
  ParKey: Variant;
begin
  if KeyField.IsNull then Exit;
  if VarIsNull(FMaxKey) or VarGT(KeyField.Value,FMaxKey)
  then FMaxKey := KeyField.Value;
  ParKey := ParentField.Value;
  if VarEQ(ParKey,KeyField.Value) then ParKey := null;
  Node := GetNodeByKey(KeyField.Value);
  Par := GetNodeByKey(ParKey);
  if Node=nil then Node := CreateDBNode(KeyField.Value,Par);
  if Node=nil then Exit;
  if VarIsNull(Node.ParentKey) then Node.FParentKey := ParKey;
  if Node.Parent<>Par then MoveDBNode(Node,Par);
  with Node do
  begin
    if TextField <> nil then
      Text := TextField.AsString;
    if WidthField <> nil then
      Width := GetIntField(WidthField,0);
    if HeightField <> nil then
      Height := GetIntField(HeightField,0);
    if ColorField <> nil then
      Color := GetIntField(ColorField,clNone);
    if ShapeField <> nil then
      Shape := TdxOcShape(GetIntField(ShapeField, Ord(shRectangle)));
    if ChAlignField <> nil then
      ChildAlign := TdxOcNodeAlign(GetIntField(ChAlignField, Ord(caCenter)));
    if ImAlignField <> nil then
      ImageAlign := TdxOcImageAlign(GetIntField(ImAlignField, Ord(iaNone)));
    if ImageField <> nil then
      ImageIndex := GetIntField(ImageField, -1);
    if Assigned(OnLoadNode) then
      OnLoadNode(Self, Node);
    FRecNum := FCurRec;
    FRefreshed := True;
  end;
end;

procedure TdxDbOrgChart.RefreshParents;
var
  Node,Par: TdxDbOcNode;
  I: Integer;
begin
  for I := 0 to FKeyList.Count-1 do
  begin
    Node := TdxDbOcNode(FKeyList[I]);
    if Node.FRefreshed and (Node.Parent = nil)  then
    begin
      Par := GetNodeByKey(Node.ParentKey);
      if (Par=nil) and not VarIsNull(Node.ParentKey) then
      begin
        if not Node.IsParentRoot then
          Node.FRefreshed := False
      end else MoveDBNode(Node, Par);
    end;
  end;
end;

function TdxDbOrgChart.FindNearest(AKey: Variant; var AIndex: Integer): Boolean;
var
  CKey: Variant;
  Min,Max: Integer;
begin
  Result := False;
  Min := 0; Max := FKeyList.Count;
  while true do begin
    AIndex := (Min + Max) shr 1;
    if Min=Max then Exit;
    CKey := TdxDbOcNode(FKeyList[AIndex]).Key;
    if VarEQ(AKey,CKey) then begin
      Result := True;
      Exit;
    end;
    if VarGT(AKey,CKey) then Min := AIndex+1
    else Max := AIndex;
  end;
end;

function TdxDbOrgChart.GetNodeByKey(AKey: Variant): TdxDbOcNode;
var
  I: Integer;
begin
  if FindNearest(AKey,I)
  then Result := TdxDbOcNode(FKeyList[I]) else Result := nil;
end;

function TdxDbOrgChart.FindIndex(AKey: Variant; AParent: TdxDbOcNode): Integer;
var
  Node: TdxDbOcNode;
begin
//  if AParent=nil then Result := RootNode.Count else Result := AParent.Count;
//  if not KeyOrder and (OrderField=nil) then Exit;
  Result := 0;
  if AParent=nil then Node := TdxDbOcNode(GetFirstNode)
  else Node := TdxDbOcNode(AParent.GetFirstChild);
  while Node <> nil do begin
    if not VarGT(AKey,Node.FOrder) then Exit;
    Inc(Result);
    Node := TdxDbOcNode(Node.getNextSibling);
  end;
end;

function TdxDbOrgChart.CreateDBNode(AKey: Variant; AParent: TdxDbOcNode): TdxDbOcNode;
var Order: Variant;
begin
  if OrderField=nil then Order := AKey else Order := OrderField.Value;
  if VarIsNull(Order) then Order := 0;
  Result := TdxDbOcNode(InternalAdd(AParent,nil,FindIndex(Order,AParent)));
  if Result<>nil then begin
    Result.FKey := AKey;
    Result.FRecNum := FCurRec;
    Result.FOrder := Order;
    if AParent<>nil then Result.FParentKey := AParent.Key;
    AddToKeyList(Result);
  end;
end;

procedure TdxDbOrgChart.MoveDBNode(ANode,AParent: TdxDbOcNode);
begin
  if (AParent=ANode) or (AParent<>nil) and AParent.HasAsParent(ANode) then Exit;
  InternalMoveTo(AParent,ANode,FindIndex(ANode.FOrder,AParent));
end;

procedure TdxDbOrgChart.AddToKeyList(ANode: TdxDbOcNode);
var
  I: Integer;
begin
  FindNearest(ANode.Key,I);
  FKeyList.Insert(I,ANode);
end;

procedure TdxDbOrgChart.DelFromKeyList(ANode: TdxDbOcNode);
var
  I: Integer;
begin
  if FindNearest(ANode.Key,I) then
    FKeyList.Delete(I);
end;

procedure TdxDbOrgChart.SetParentNode(ANode: TdxDbOcNode);
begin
  if VarIsNull(ANode.ParentKey) and ParentField.Required then
    ParentField.Value := ANode.Key
  else
    ParentField.Value := ANode.ParentKey;
end;

function TdxDbOrgChart.InternalAdd(AParent:TdxOcNode; Data: TdxOcNodeData; Idx:Integer): TdxOcNode;

  function IsAutoIncField(AField: TField): Boolean;
  begin
    Result := (AField is TAutoIncField) or (AField.AutoGenerateValue = arAutoInc);
  end;

var
  Node: TdxDbOcNode;
  Bm: Integer;
begin
  Result := nil;
  if not Active or FEnableDB and not DataSet.CanModify then
    Exit;

  Result := inherited InternalAdd(AParent, Data, Idx);
  if FEnableDB and (Result <> nil) then
  begin
    Bm := FCurRec;
    FEnableDB := False;
    Node := TdxDbOcNode(Result);
    if not IsAutoIncField(KeyField) then
      Node.FKey := NewKey;
    if AParent <> nil then
      Node.FParentKey := TdxDbOcNode(AParent).Key;
    if Node.Index = 0 then
      Node.FOrder := 0
    else
      Node.FOrder := TdxDbOcNode(Node.getPrevSibling).FOrder+1;

    try
      DataSet.Append;
      Inc(FRecCount);
      FCurRec := FRecCount;
      Node.FRecNum := FCurRec;
      if not IsAutoIncField(KeyField) then
        KeyField.Value := Node.Key;
      SetParentNode(Node);
      if OrderField <> nil then
        OrderField.Value := Node.FOrder;
      DataSet.Post;
      if IsAutoIncField(KeyField) then
        Node.FKey := KeyField.Value;
      if OrderField = nil then
        Node.FOrder := Node.FKey;
      CheckRec(Node);
      AddToKeyList(Node);
      CheckKeys(Node);
    finally
      SetBookmark(Bm);
      FEnableDB := True;
    end;
  end;
end;

procedure TdxDbOrgChart.InternalMoveTo(AParent,ANode: TdxOcNode; Idx: Integer);
var
  Node: TdxDbOcNode;
  Bm: Integer;
begin
  if FEnableDB and not DataSet.CanModify then Exit;
  inherited InternalMoveTo(AParent,ANode,Idx);
  Node := TdxDbOcNode(ANode);
  if Node.Parent=nil then Node.FParentKey := null
  else Node.FParentKey := TdxDbOcNode(Node.Parent).Key;
  if FEnableDB then begin
    Bm := FCurRec;
    FEnableDB := False;
    try
      if LocateToNode(Node) then
      begin
        DataSet.Edit;
        SetParentNode(Node);
        DataSet.Post;
        CheckRec(Node);
      end;
      if Node.Index = 0 then
        CheckKeys(Node)
      else
        CheckKeys(TdxDbOcNode(Node.getPrevSibling));
    finally
      SetBookmark(Bm);
      FEnableDB := True;
    end;
  end;
end;

function TdxDbOrgChart.LocateToNode(ANode: TdxDbOcNode): Boolean;
begin
  SetBookmark(ANode.FRecNum);
  Result := (FCurRec=ANode.FRecNum) and VarEQ(ANode.Key,KeyField.Value);
  if not Result then
    Result := DataSet.Locate(KeyFieldName, ANode.Key, []);
end;

procedure TdxDbOrgChart.CheckKeys(ANode: TdxDbOcNode);
var
  Node: TdxDbOcNode;
  I: Integer;
  Loc: Boolean;
begin
  if not KeyOrder and (OrderField=nil) then Exit;
  Node := TdxDbOcNode(ANode.getNextSibling);
  if Node=nil then Exit;
  if VarGE(ANode.FOrder,Node.FOrder) then begin
    Loc := LocateToNode(Node);
    if OrderField<>nil then Node.FOrder := ANode.Forder+1
    else begin
      Node.FKey := NewKey;
      Node.FOrder := Node.Key;
    end;
    if Loc then begin
      DataSet.Edit;
      if OrderField=nil then KeyField.Value := Node.Key
      else OrderField.Value := Node.FOrder;
      DataSet.Post;
      CheckRec(Node);
    end;
    if OrderField=nil then begin
      FKeyList.Remove(Node);
      FKeyList.Insert(FKeyList.Count,Node);
      for I := 0 to Node.Count-1 do begin
        TdxDbOcNode(Node[I]).FParentKey := Node.Key;
        if LocateToNode(TdxDbOcNode(Node[I])) then begin
          DataSet.Edit;
          ParentField.Value := Node.Key;
          DataSet.Post;
          CheckRec(Node);
        end;
      end;
    end;
  end;
  CheckKeys(Node);
end;

procedure TdxDbOrgChart.CheckRec(ANode: TdxDbOcNode);
var
  Node: TdxDbOcNode;
  I,OldNum,NewNum: Integer;
begin
  OldNum := ANode.FRecNum;
  NewNum := DataSet.RecNo-1;
  FCurRec := NewNum;
  if NewNum < 0 then begin
    NewNum := -DataSet.MoveBy(-99999999);
    FCurRec := 0;
  end;
  if OldNum=NewNum then Exit;
  for I := 0 to FKeyList.Count-1 do begin
    Node := TdxDbOcNode(FKeyList[I]);
    if Node.FRecNum > OldNum then Dec(Node.FRecNum);
    if Node.FRecNum >= NewNum then Inc(Node.FRecNum);
  end;
  ANode.FRecNum := NewNum;
end;

function TdxDbOrgChart.NewKey: Variant;
begin
  Result := null;
  if Assigned(OnNewKey) then begin
    OnNewKey(Self,FMaxKey,Result);
    if not VarGT(Result,FMaxKey) then Result := null;
  end;
  if VarIsNull(Result) and IsIntegerField(KeyField) then
    if VarIsNull(FMaxKey) then Result := 0
    else Result := FMaxKey+1;
  if not VarIsNull(Result) then FMaxKey := Result
  else raise EVariantError.Create('Cannot create new key.');
end;

procedure TdxDbOrgChart.Delete(ANode: TdxOcNode);
var
  Node: TdxDbOcNode;
  Bm: Integer;
begin
  if ANode=nil then Exit;
  if FEnableDB and Active then begin
    if not DataSet.CanModify then Exit;
    FEnableDB := False;
    Bm := FCurRec;
    try
      if LocateToNode(TdxDbOcNode(ANode)) then begin
        DataSet.Delete;
        Dec(FRecCount);
        if FCurRec > FRecCount then FCurRec := FRecCount;
        if Bm > FCurRec then Dec(Bm);
        Node := TdxDbOcNode(GetFirstNode);
        while Node<>nil do begin
          if Node.FRecNum > FCurRec then Dec(Node.FRecNum);
          if (Node=ANode.Parent) and Node.Deleting then Node := TdxDbOcNode(ANode);
          Node := TdxDbOcNode(Node.GetNext);
        end;
      end;
    finally
      SetBookmark(Bm);
      FEnableDB := True;
    end;
  end;
  if FKeyList<>nil then DelFromKeyList(TdxDbOcNode(ANode));
  inherited Delete(ANode);
end;

procedure TdxDbOrgChart.DoScroll(Dist: Integer);
var Sel: TdxDbOcNode;
begin
  if FEnableDB and (KeyField<>nil) then begin
    FEnableDB := False;
    FCurRec := FCurRec + Dist;
    Sel := GetNodeByKey(KeyField.Value);
    if (Sel <> nil) and not VarEQ(KeyField.Value, Sel.Key) then Sel := nil;
    Selected := Sel;
    if Sel <> nil then Sel.MakeVisible;
    FEnableDB := True;
  end;
end;

procedure TdxDbOrgChart.RecordChanged(Field: TField);
begin
  if FEnableDB and (Selected<>nil) and (Field<>nil) and (DataSet.State=dsEdit) then begin
    FEnableDB := False;
    if Field = TextField then Selected.Text := TextField.AsString;
    if Field = ImageField then Selected.ImageIndex := ImageField.AsInteger;
    FEnableDB := True;
  end;
end;

procedure TdxDbOrgChart.NodeChanged(ANode: TdxOcNode);
var
  Bm: Integer;
  Info: TdxOcNodeInfo;
begin
  if FEnableDB and DataSet.CanModify then begin
    FEnableDB := False;
    Bm := FCurRec;
    try
      if LocateToNode(TdxDbOcNode(ANode)) then begin
        ANode.GetNodeInfo(Info);
        DataSet.Edit;
        if TextField<>nil then TextField.AsString := ANode.Text;
        if WidthField<>nil then WidthField.AsInteger := Info.Width;
        if HeightField<>nil then HeightField.AsInteger := Info.Height;
        if ColorField<>nil then ColorField.AsInteger := Info.Color;
        if ShapeField<>nil then ShapeField.AsInteger := Ord(Info.Shape);
        if ChAlignField<>nil then ChAlignField.AsInteger := Ord(Info.Align);
        if ImAlignField<>nil then ImAlignField.AsInteger := Ord(Info.IAlign);
        if ImageField<>nil then ImageField.AsInteger := Info.Index;
        DataSet.Post;
	CheckRec(TdxDbOcNode(ANode));
      end;
    finally
      SetBookmark(Bm);
      FEnableDB := True;
    end;
  end;
end;

procedure TdxDbOrgChart.ActiveChanged;
begin
  FKeyField := nil;
  FParentField := nil;
  FTextField := nil;
  FWidthField := nil;
  FHeightField := nil;
  FColorField := nil;
  FShapeField := nil;
  FChAlignField := nil;
  FImIndexField := nil;
  FImAlignField := nil;
  FEnableDB := False;
  Clear;
  FEnableDB := True;
  if DataSet<>nil then begin
    FLink.FFiltered := DataSet.Filtered;
    FLink.FFilter := DataSet.Filter;
  end;
  AssignFields;
  if Active then DataChanged;
end;

procedure TdxDbOrgChart.DataChanged;
begin
  if not FEnableDB or (DataSet.State in [dsEdit,dsInsert]) then Exit;
  RefreshItems;
  DoScroll(0);
end;

procedure TdxDbOrgChart.DoChange(Node: TdxOcNode);
begin
  if FEnableDB and (Node<>nil) then begin
    FEnableDB := False;
    try
      LocateToNode(TdxDbOcNode(Node));
    except
    end;
    FEnableDB := True;
  end;
  inherited DoChange(Node);
end;

procedure TdxDbOrgChart.DoChanging(Node: TdxOcNode; var Allow: Boolean);
var Bm: Integer;
begin
  inherited DoChanging(Node,Allow);
  if Allow and FEnableDB and (Node<>nil) then begin
    Bm := FCurRec;
    FEnableDB := False;
    try
      Allow := LocateToNode(TdxDbOcNode(Node));
    except
      Allow := False;
    end;
    if not Allow then SetBookmark(Bm);
    FEnableDB := True;
  end;
end;

function TdxDbOrgChart.GetNodeClass: TdxOcNodeClass;
begin
  Result := TdxDbOcNode;
end;

procedure TdxDbOrgChart.SetDataSource(Value: TDataSource);
begin
  FLink.DataSource := Value;
end;

procedure TdxDbOrgChart.SetKeyFieldName(const Value: String);
begin
  if FKeyFieldName <> Value then begin
    FKeyFieldName := Value;
    ActiveChanged;
  end;
end;

procedure TdxDbOrgChart.SetParentFieldName(const Value: String);
begin
  if FParentFieldName <> Value then begin
    FParentFieldName := Value;
    ActiveChanged;
  end;
end;

procedure TdxDbOrgChart.SetTextFieldName(const Value: String);
begin
  if FTextFieldName <> Value then begin
    FTextFieldName := Value;
    ActiveChanged;
  end;
end;

procedure TdxDbOrgChart.SetOrderFieldName(const Value: String);
begin
  if FOrderFieldName <> Value then begin
    FOrderFieldName := Value;
    ActiveChanged;
  end;
end;

procedure TdxDbOrgChart.SetWidthFieldName(const Value: String);
begin
  if FWidthFieldName <> Value then begin
    FWidthFieldName := Value;
    ActiveChanged;
  end;
end;

procedure TdxDbOrgChart.SetHeightFieldName(const Value: String);
begin
  if FHeightFieldName <> Value then begin
    FHeightFieldName := Value;
    ActiveChanged;
  end;
end;

procedure TdxDbOrgChart.SetColorFieldName(const Value: String);
begin
  if FColorFieldName <> Value then begin
    FColorFieldName := Value;
    ActiveChanged;
  end;
end;

procedure TdxDbOrgChart.SetShapeFieldName(const Value: String);
begin
  if FShapeFieldName <> Value then begin
    FShapeFieldName := Value;
    ActiveChanged;
  end;
end;

procedure TdxDbOrgChart.SetChAlignFieldName(const Value: String);
begin
  if FChAlignFieldName <> Value then begin
    FChAlignFieldName := Value;
    ActiveChanged;
  end;
end;

procedure TdxDbOrgChart.SetImIndexFieldName(const Value: String);
begin
  if FImIndexFieldName <> Value then begin
    FImIndexFieldName := Value;
    ActiveChanged;
  end;
end;

procedure TdxDbOrgChart.SetImAlignFieldName(const Value: String);
begin
  if FImAlignFieldName <> Value then begin
    FImAlignFieldName := Value;
    ActiveChanged;
  end;
end;

function TdxDbOrgChart.CreateEditor: TdxOcInplaceEdit;
begin
  Result := inherited CreateEditor;
  if TextField<>nil then Result.MaxLength := TextField.Size;
end;

end.
