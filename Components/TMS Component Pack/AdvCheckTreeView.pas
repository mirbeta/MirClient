{***************************************************************************}
{ TAdvCheckTreeview component                                               }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2013 - 2015                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit AdvCheckTreeView;

interface


uses
  SysUtils, Classes, Controls, ComCtrls, Windows, Messages, CommCtrl;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 1; // Build nr.

  // v1.0.0.0 : First release
  // v1.0.0.1 : Fixed: Issue with presetting checkboxes in form create
  // v1.0.0.2 : Fixed: Issue with checking hierarchy
  // v1.0.1.0 : New : Event OnNodeCheckedChanging event added
  // v1.0.1.1 : Fixed : Issue with disabled nodes
  //          : Fixed : Issue with OnNodeCheckedChanging

type
  TCheckHierarchy = (chCheckParent, chCheckChilds);

  TCheckHierarchySet = set of TCheckHierarchy;
  TTreeViewCheckChangedEvent = procedure (Sender: TObject; Node:TTreeNode; NewState:Boolean) of object;

  TTreeViewCheckChangingEvent = procedure (Sender: TObject; Node:TTreeNode; NewState:Boolean;var Allow: boolean) of object;

  TAdvCheckTreeView = class(TTreeView)
  private
    FIsBusy: boolean;
    FCheckBoxes: boolean;
    FNodeCheckedChanged: TTreeViewCheckChangedEvent;
    FNodeCheckedChanging: TTreeViewCheckChangingEvent;
    FCheckHierarchy: TCheckHierarchySet;
    procedure PerformCheck(Node:TTreeNode);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure CNNotify(var message: TWMNotify); message CN_NOTIFY;
  protected
    procedure DoNodeCheckedChanged(Node:TTreeNode; NewState:Boolean); virtual;
    procedure DoNodeCheckedChanging(Node:TTreeNode; NewState:Boolean; var Allow: boolean); virtual;
    function GetChecked(node : TTreeNode):boolean;
    procedure SetChecked(Node:TTreeNode; value : Boolean);
    procedure SetCheckBoxes(const Value: boolean);
    procedure CreateWnd; override;
    function GetVersionNr: Integer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    property Checked[Node: TTreeNode]: boolean read GetChecked write SetChecked;
  published
    property CheckHierarchy: TCheckHierarchySet read FCheckHierarchy write FCheckHierarchy default [];
    property CheckBoxes: boolean read FCheckBoxes write SetCheckBoxes default true;
    property OnNodeCheckedChanged: TTreeViewCheckChangedEvent read FNodeCheckedChanged write FNodeCheckedChanged;
    property OnNodeCheckedChanging: TTreeViewCheckChangingEvent read FNodeCheckedChanging write FNodeCheckedChanging;
    property Version: string read GetVersion write SetVersion;
 end;


implementation

const
  IIL_UNCHECKED = 1;
  IIL_CHECKED   = 2;

{$IFNDEF DELPHI_UNICODE}
const
  {$EXTERNALSYM TVN_ITEMCHANGINGA}
  TVN_ITEMCHANGINGA       = TVN_FIRST-16;

  {$EXTERNALSYM TVN_ITEMCHANGING}
  TVN_ITEMCHANGING        = TVN_ITEMCHANGINGA;

type
  { $EXTERNALSYM tagNMTVITEMCHANGE}
  tagNMTVITEMCHANGE = packed record
    hdr: NMHDR;
    uChanged: UINT;
    hItem: HTREEITEM;
    uStateNew: UINT;
    uStateOld: UINT;
    lParam: LPARAM;
  end;
  PNMTVItemChange = ^TNMTVItemChange;
  TNMTVItemChange = tagNMTVITEMCHANGE;
{$ENDIF}

{ TAdvCheckTreeView }

procedure TAdvCheckTreeView.CNNotify(var message: TWMNotify);
var
  ITCHG: tagNMTVITEMCHANGE;
  nchk,ochk: boolean;
  Node: TTreeNode;
  Allow: boolean;
begin

  if message.NMHdr^.Code = TVN_ITEMCHANGING then
  begin
    ITCHG := PNMTVITEMCHANGE(pointer(message.nmhdr))^;

    if ITCHG.uChanged = TVIF_STATE then
    begin
      nchk := (ITCHG.uStateNew and UINT(INDEXTOSTATEIMAGEMASK(IIL_CHECKED)) = UINT(INDEXTOSTATEIMAGEMASK(IIL_CHECKED)));
      ochk := (ITCHG.uStateOld and UINT(INDEXTOSTATEIMAGEMASK(IIL_CHECKED)) = UINT(INDEXTOSTATEIMAGEMASK(IIL_CHECKED)));

      if nchk <> ochk then
      begin
        Node := Items.GetNode(ITCHG.hItem);

        if Assigned(Node) then
        begin
          Allow := true;
          //DoNodeCheckedChanging(Node ,nchk, Allow);

          if not Allow then
          begin
            message.Result := 1;
            Exit;
          end;
        end;
      end;
    end;
  end;
  inherited;
end;

constructor TAdvCheckTreeView.Create(AOwner: TComponent);
begin
  inherited;
  FCheckHierarchy := [];
  FCheckBoxes := true;
end;

procedure TAdvCheckTreeView.CreateWnd;
var
  st: integer;
begin
  inherited;

  if FCheckBoxes then
  begin
    st := GetWindowLong(Handle,GWL_STYLE);
    st := st OR TVS_CHECKBOXES;
    SetWindowLong(handle, GWL_STYLE, st);
  end;
end;

procedure TAdvCheckTreeView.DoNodeCheckedChanged(Node: TTreeNode;
  NewState: Boolean);
begin
  PerformCheck(Node);
  if Assigned(FNodeCheckedChanged) then
    FNodeCheckedChanged(Self,Node,NewState);
end;

procedure TAdvCheckTreeView.DoNodeCheckedChanging(Node: TTreeNode;
  NewState: Boolean; var Allow: boolean);
begin
  if Assigned(OnNodeCheckedChanging) then
    OnNodeCheckedChanging(Self, Node, NewState, Allow);
end;

function TAdvCheckTreeView.GetChecked(node : TTreeNode): boolean;
var
  Item: TTvItem;
begin
  Item.hItem := Node.ItemId;
  Item.Mask := TVIF_STATE;
  Item.StateMask := TVIS_STATEIMAGEMASK;
  if not TreeView_GetItem(Handle, Item) then
    Abort;

  Result := ((integer(Item.State) and INDEXTOSTATEIMAGEMASK(IIL_CHECKED)) =
    INDEXTOSTATEIMAGEMASK(IIL_CHECKED));
end;

function TAdvCheckTreeView.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) +
    '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvCheckTreeView.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvCheckTreeView.PerformCheck(Node: TTreeNode);
var
  state: boolean;
  i: integer;
  chk: boolean;
  tn: TTreeNode;
begin
  if FIsBusy then
    Exit;

  {$IFDEF DELPHI_UNICODE}
  if not Node.Enabled  then
    Exit;
  {$ENDIF}

  state := GetChecked(Node);

  FIsBusy := true;

  if (chCheckChilds in FCheckHierarchy) {and state} then
  begin
    for i := 0 to Node.Count -1 do
      SetChecked(Node[i],state);
  end;

  if (chCheckParent in FCheckHierarchy) and state then
  begin
    chk := true;
    tn := Node.Parent;

    if Assigned(tn) then
    begin
      for i := 0 to tn.Count - 1 do
      begin
        if not GetChecked(tn[i]) then
        begin
          chk := false;
          break;
        end;
      end;

      if chk then SetChecked(tn,true);
    end;
  end;

  if (chCheckParent in FCheckHierarchy) and not state then
    if Node.Parent <> nil then
    begin
      chk := true;
      for i := 0 to Node.Parent.Count -1 do
      begin
        if not GetChecked(Node.Parent[i]) then
        begin
          chk := false;
          break;
        end;
      end;
      if not chk then SetChecked(Node.Parent,false);
    end;

  FIsBusy := false;
end;

procedure TAdvCheckTreeView.SetCheckBoxes(const Value: boolean);
begin
  if (Value <> FCheckBoxes) then
  begin
    FCheckBoxes := Value;
    RecreateWnd;
  end;
end;

procedure TAdvCheckTreeView.SetChecked(Node: TTreeNode; Value: Boolean);
var
  Item: TTvItem;
begin
  if Value = GetChecked(Node) then
    Exit;

  FillChar(Item, SizeOf(Item), 0);

  Item.hItem := Node.ItemId;
  Item.Mask := TVIF_STATE;
  Item.StateMask := TVIS_STATEIMAGEMASK;

  if not TreeView_GetItem(Handle, Item) then
    Exit;

  if Value then
    Item.State := INDEXTOSTATEIMAGEMASK(IIL_CHECKED)
  else
    Item.State := INDEXTOSTATEIMAGEMASK(IIL_UNCHECKED);

  TreeView_SetItem(Handle,Item);

  PerformCheck(Node);
end;

procedure TAdvCheckTreeView.SetVersion(const Value: string);
begin
  // read-only property
end;

procedure TAdvCheckTreeView.WMKeyDown(var Message: TWMKeyDown);
var
  tn1,tn2: TTreeNode;
  b, Allow: boolean;
begin
  if message.CharCode <> VK_SPACE then
  begin
    inherited;
    Exit;
  end;

  b := false;
  tn1 := Selected;
  if Assigned(tn1) then
    b := GetChecked(tn1);

  {$IFDEF DELPHI_UNICODE}
  if Assigned(tn1) and tn1.Enabled then
  {$ENDIF}
  begin
    Allow := true;
    DoNodeCheckedChanging(tn1 ,b, Allow);

    if Allow then
    begin
      inherited;

      tn2 := Selected;
      if (tn1 <> nil) and (tn2 <> nil) and (tn1 = tn2) and (b <> GetChecked(tn2)) then
        DoNodeCheckedChanged(tn2,not b);
    end;
  end;
end;

procedure TAdvCheckTreeView.WMLButtonDblClk(var Message: TWMLButtonDblClk);
var
  tn1, tn2: TTreeNode;
  b, Allow: boolean;
begin
  tn1 := GetNodeAt(Message.XPos, Message.YPos);

  b := false;
  if Assigned(tn1) then
    b := GetChecked(tn1);

  {$IFDEF DELPHI_UNICODE}
  if Assigned(tn1) and tn1.Enabled then
  {$ENDIF}
  begin
    Allow := true;
    DoNodeCheckedChanging(tn1 ,b, Allow);

    if Allow then
    begin
      inherited;

      tn2 := GetNodeAt(Message.XPos,Message.YPos);

      if (tn1 <> nil) and (tn2 <> nil) and (tn1 = tn2) and (b <> GetChecked(tn2)) then
        DoNodeCheckedChanged(tn2,not b);
    end;
  end;
end;

procedure TAdvCheckTreeView.WMLButtonDown(var Message: TWMLButtonDown);
var
  tn1,tn2: TTreeNode;
  b, Allow: boolean;
begin
  b := false;
  tn1 := GetNodeAt(Message.XPos, Message.YPos);
  if Assigned(tn1) then
    b := GetChecked(tn1);

  {$IFDEF DELPHI_UNICODE}
  if Assigned(tn1) and tn1.Enabled then
  {$ENDIF}
  begin
    Allow := true;
    DoNodeCheckedChanging(tn1 ,b, Allow);

    if Allow then
    begin
      inherited;

      tn2 := GetNodeAt(Message.XPos,Message.YPos);

      if (tn1 <> nil) and (tn2 <> nil) and (tn1 = tn2) and (b <> GetChecked(tn2)) then
        DoNodeCheckedChanged(tn2,not b);
    end;
  end;
end;

end.
