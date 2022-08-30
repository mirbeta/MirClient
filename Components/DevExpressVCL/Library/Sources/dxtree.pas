{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express tree view control                                }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSGRID AND ALL ACCOMPANYING VCL  }
{   CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.                  }
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

unit dxtree;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, CommCtrl, ExtCtrls;

type
  TdxTreeViewOption = (trCanDelete, trConfirmDelete);
  TdxTreeViewOptions = set of TdxTreeViewOption;

  TDragDropTreeNode = procedure(Destination, Source: TTreeNode;
    var Accept: Boolean) of object;
  TEndDragTreeNode = procedure(Destination, Source: TTreeNode;
    var AttachMode: TNodeAttachMode) of object;
  TDragDropIsCopy = procedure(Destination, Source: TTreeNode;
    var IsCopy: Boolean) of object;
  TTreeViewCustomDraw = procedure(Sender: TObject; TreeNode: TTreeNode;
    AFont: TFont; var AColor, ABkColor: TColor) of object;

  // Avoid MoveTo bug in D4.
  TdxTreeNode = class(TTreeNode)
  public
    procedure MoveTo(Destination: TTreeNode; Mode: TNodeAttachMode); override;
  end;

  TCustomdxTreeView = class(TCustomTreeView)
  private
    FOptions: TdxTreeViewOptions;
    FSelectedIndex: Integer;
    FDragDropProcessingFlag: Boolean;
    FDragObject: TDragObject;
    FDeletingConfirmationMessage: string;
    FIsConfirmationAssigned: Boolean;
    FScrollTimerID: Integer;
    fntcd: TFont;
    FCanvas: TControlCanvas;
    FOnDragDropTreeNode: TDragDropTreeNode;
    FOnEndDragTreeNode: TEndDragTreeNode;
    FOnDragDropIsCopy: TDragDropIsCopy;
    FOnCustomDraw: TTreeViewCustomDraw;
    FOnSetEditText: TTVEditedEvent;
    FShowNodeHint: Boolean;
    FInternalDragObject: TDragObject;

    function GetDragSourceTreeNode: TTreeNode;
    function GetDeletingConfirmationMessage: string;
    procedure SetSelectedIndex(Value: Integer);
    procedure SetDeletingConfirmationMessage(const AValue: string);
    procedure CMDrag(var Message: TCMDrag); message CM_DRAG;
    function DoCNNotify(var Message: TWMNotify): Boolean;
    function DoWMNotify(var Message: TWMNotify): Boolean;
    procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure WMNotify(var Message: TWMNotify); message WM_NOTIFY;
    procedure ReadIsAssigned(AReader: TReader);
    procedure WriteIsAssigned(AWriter: TWriter);
  protected
    CopyTreeNodeStructFlag: Boolean;

    procedure DefineProperties(AFiler: TFiler); override;

    procedure DoCustomDraw(TreeNode: TTreeNode; AFont: TFont;
      var AColor, ABkColor: TColor); virtual;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    function GetNodeFromItem(const Item: TTVItem): TTreeNode;
    function GetListItemText(TreeNode: TTreeNode): String; virtual;
    procedure InsertTreeNodeStructure(ListS, ListD: TList); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoDragDropTreeNode(Destination, Source: TTreeNode; var Accept: Boolean); virtual;
    procedure DoDragDropIsCopy(Destination, Source: TTreeNode; var IsCopy: Boolean); virtual;

    property Options: TdxTreeViewOptions read FOptions write FOptions
      default [trCanDelete, trConfirmDelete];
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CreateNode: TTreeNode; override;
    procedure SetFocus; override;

    procedure GetNodeStructure(TreeNode: TTreeNode; List: TList); virtual;
    function IsCustomDraw: Boolean; virtual;
    function MoveTreeNodeStructure(Source, Destination: TTreeNode; IsCopy: Boolean): TTreeNode;
    procedure UpdateSizeGripCorner; virtual;
    property DragSourceTreeNode: TTreeNode read GetDragSourceTreeNode;
    property IsConfirmationAssigned: Boolean read FIsConfirmationAssigned write FIsConfirmationAssigned;
    property OnSetEditText: TTVEditedEvent read FOnSetEditText write FOnSetEditText;

    property Color;
    property DragCursor;
    property Font;
    property Images;
    property Items;
    property Indent;
    property PopupMenu;
    property ReadOnly;
    property ShowButtons;
    property ShowLines;
    property ShowRoot;
    property SortType;
    property StateImages;

    property OnClick;
    property OnChanging;
    property OnDblClick;
    property OnGetImageIndex;
    property OnGetSelectedIndex;
  published
    property ImeMode;
    property ImeName;
    property RightClickSelect;
    property ShowNodeHint: Boolean read FShowNodeHint write FShowNodeHint;
    property OnCustomDraw: TTreeViewCustomDraw read FOnCustomDraw write FOnCustomDraw;
    property OnDragDropIsCopy: TDragDropIsCopy read FOnDragDropIsCopy write FOnDragDropIsCopy;
    property OnDragDropTreeNode: TDragDropTreeNode read FOnDragDropTreeNode
      write FOnDragDropTreeNode;
    property OnEndDragTreeNode: TEndDragTreeNode read FOnEndDragTreeNode
      write FOnEndDragTreeNode;

    property Anchors;
    property AutoExpand;
    property BiDiMode;
    property Constraints;
    property DeletingConfirmationMessage: string read GetDeletingConfirmationMessage write SetDeletingConfirmationMessage stored FIsConfirmationAssigned;
    property DragKind;
    property HotTrack;
    property ParentBiDiMode;
    property RowSelect;
    property ToolTips;
    property OnCustomDrawItem;
    property OnEndDock;
    property OnStartDock;
    property OnAdvancedCustomDraw;
  end;

  TdxTreeView = class(TCustomdxTreeView)
  published
    property ShowButtons;
    property BorderStyle;
    property DragCursor;
    property ShowLines;
    property ShowRoot;
    property ReadOnly;
    property DragMode;
    property HideSelection;
    property Indent;
    property Items;
    property OnEditing;
    property OnEdited;
    property OnExpanding;
    property OnExpanded;
    property OnCollapsing;
    property OnCompare;
    property OnCollapsed;
    property OnChanging;
    property OnChange;
    property OnDeletion;
    property OnGetImageIndex;
    property OnGetSelectedIndex;
    property Align;
    property Enabled;
    property Font;
    property Color;
    property ParentColor;
    property ParentCtl3D;
    property Ctl3D;
    property Options;
    property SelectedIndex;
    property SortType;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnDragDrop;
    property OnDragOver;
    property OnStartDrag;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property PopupMenu;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property Images;
    property StateImages;
  end;

resourcestring
  dxTreeViewDelConfirm = 'Do you want to delete the item "%s"';
  dxDBTreeViewSmartLoadS = 'Set trSmartRecordLoad in the Options property to decrease the loading time. Do you want to do it now?';

implementation

uses
  ImgList;

{$I dxcmctl.inc}
{$R dxtree.res}

const
  crdxTreeDrag = -1011;

var
  dxTreeViewDragImages: TDragImageList;
  DropSourceTreeNode: TTreeNode;
  OldDragOverTreeNode: TTreeNode;

{TdxTreeNode}
//Avoid MoveTo bug in D4.
procedure TdxTreeNode.MoveTo(Destination: TTreeNode; Mode: TNodeAttachMode);
var
  Dummy: TTreeNode;
begin
  if Mode in [naAddChild, naAddChildFirst] then
  begin
    TCustomdxTreeView(TreeView).CopyTreeNodeStructFlag := True;
    if Mode = naAddChildFirst then
      Dummy := TTreeView(Destination.TreeView).Items.AddChildFirst(Destination, '')
    else
      Dummy := TTreeView(Destination.TreeView).Items.AddChild(Destination, '');
    TCustomdxTreeView(TreeView).CopyTreeNodeStructFlag := False;
    inherited MoveTo(Dummy, naInsert);
    TCustomdxTreeView(TreeView).CopyTreeNodeStructFlag := True;
    Dummy.Free;
    TCustomdxTreeView(TreeView).CopyTreeNodeStructFlag := False;
  end
  else
    inherited MoveTo(Destination, Mode);
end;

{TCustomdxTreeView}
constructor TCustomdxTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScrollTimerID := -1;
  FSelectedIndex := -1;
  FOptions := [trCanDelete, trConfirmDelete];
  FDragDropProcessingFlag := False;
  fntcd := TFont.Create;
  FCanvas := TControlCanvas.Create;
  FShowNodeHint := True;
end;

destructor TCustomdxTreeView.Destroy;
begin
  fntcd.Free;
  FCanvas.Free;
  inherited;
end;

type
  TCustomdxTreeViewDragObject = class(TDragControlObject)
  protected
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; override;
  end;

function TCustomdxTreeViewDragObject.GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor;
begin
  if not Accepted then
    Result := crNoDrop
  else
    if not (GetKeyState(VK_CONTROL) < 0) then
      Result := TCustomdxTreeView(Control).DragCursor
    else
      Result := crdxTreeDrag;
end;

procedure TCustomdxTreeView.DoStartDrag(var DragObject: TDragObject);
begin
  DragObject := TCustomdxTreeViewDragObject.Create(Self);
  FInternalDragObject := DragObject;
  inherited;
  dxTreeViewDragImages := GetDragImages;
  if Self.Focused then
    DropSourceTreeNode := Selected
  else
    DropSourceTreeNode := nil;
end;

procedure TCustomdxTreeView.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  inherited DoEndDrag(Target, X, Y);
  FInternalDragObject.Free;
  DropSourceTreeNode := nil;
end;

procedure TCustomdxTreeView.DoDragDropTreeNode(Destination, Source: TTreeNode;
  var Accept: Boolean);
begin
  if Assigned(FOnDragDropTreeNode) then
    FOnDragDropTreeNode(Destination, Source, Accept);
end;

procedure TCustomdxTreeView.DoDragDropIsCopy(Destination, Source: TTreeNode; var IsCopy: Boolean);
begin
 if Assigned(FOnDragDropIsCopy) then
    FOnDragDropIsCopy(Destination, Source, IsCopy);
end;

procedure TCustomdxTreeView.GetNodeStructure(TreeNode: TTreeNode; List: TList);

  procedure GetNodeStructure_(TreeNode: TTreeNode; List: TList);
  var
    tn: TTreeNode;
  begin
    if (TreeNode <> nil) and TreeNode.HasChildren then
    begin
      tn := TreeNode.GetFirstChild;
      while tn <> nil do
      begin
        List.Add(tn);
        if tn.HasChildren then
          GetNodeStructure_(tn, List);
        tn := TreeNode.GetNextChild (tn);
      end;
    end;
  end;

begin
  List.Add(TreeNode);
  GetNodeStructure_(TreeNode, List);
end;

procedure TCustomdxTreeView.InsertTreeNodeStructure(ListS, ListD: TList);
begin
end;

function TCustomdxTreeView.GetDragSourceTreeNode: TTreeNode;
begin
  if (DropSourceTreeNode <> nil) and (DropSourceTreeNode.TreeView = Self) then
    Result := DropSourceTreeNode
  else
    Result := nil;
end;

function TCustomdxTreeView.GetDeletingConfirmationMessage: string;
begin
  Result := FDeletingConfirmationMessage;
  if not FIsConfirmationAssigned then
    Result := dxTreeViewDelConfirm;
end;

procedure TCustomdxTreeView.SetSelectedIndex(Value: Integer);
var
  i: Integer;
begin
  if Value <> FSelectedIndex then
  begin
    FSelectedIndex := Value;
    for i := 0 to Items.Count - 1 do
      Items[i].SelectedIndex := FSelectedIndex;
  end;
end;

procedure TCustomdxTreeView.SetDeletingConfirmationMessage(const AValue: string);
begin
  FDeletingConfirmationMessage := AValue;
  IsConfirmationAssigned := True;
end;

function TCustomdxTreeView.CreateNode: TTreeNode;
begin
  Result := TdxTreeNode.Create(Items);
  if (Result <> nil) and (FSelectedIndex <> -1) then
    Result.SelectedIndex := FSelectedIndex;
end;

procedure TCustomdxTreeView.ReadIsAssigned(AReader: TReader);
begin
  FIsConfirmationAssigned := AReader.ReadBoolean;
end;

procedure TCustomdxTreeView.WriteIsAssigned(AWriter: TWriter);
begin
  AWriter.WriteBoolean(FIsConfirmationAssigned);
end;

procedure TCustomdxTreeView.SetFocus;
begin
  if (GetParentForm(Self) <> nil) or (ParentWindow <> 0) then
    inherited SetFocus;
end;

procedure TCustomdxTreeView.KeyDown(var Key: Word; Shift: TShiftState);
var
  St: string;
begin
  if (Key = VK_DELETE) and (trCanDelete in Options)
    and not ReadOnly and (Selected <> nil) and not IsEditing then
  begin
    if IsConfirmationAssigned then
      St := Format(DeletingConfirmationMessage, [Selected.Text])
    else
      St := Format(dxTreeViewDelConfirm, [Selected.Text]);
    if not (trConfirmDelete in Options) or
     (MessageDlg(St, mtConfirmation, mbOKCancel, 0) <> idCancel) then
     Items.Delete(Selected);
  end;
  inherited;
end;

procedure TCustomdxTreeView.CMDrag(var Message: TCMDrag);
var
 lpht: TTVHitTestInfo;
 TreeNode: TTreeNode;

 procedure HideDragImage;
 begin
   if dxTreeViewDragImages <> nil then
     dxTreeViewDragImages.HideDragImage
   else
     if FDragObject <> nil then
       FDragObject.HideDragImage;
 end;

 procedure ShowDragImage;
 begin
   if dxTreeViewDragImages <> nil then
     dxTreeViewDragImages.ShowDragImage
   else
     if FDragObject <> nil then
       FDragObject.ShowDragImage;
 end;

 function IsNodeVisible(TreeNode: TTreeNode): Boolean;
 var
   Rect: TRect;
 begin
   Result := TreeView_GetItemRect(Handle, TreeNode.ItemId, Rect, False);
   Result := Result and (Rect.Top < ClientHeight);
 end;

var
  CorrectDragFlag, IsCopyFlag: Boolean;
begin
  CorrectDragFlag := False;
  with Message, DragRec^ do
  begin
    case DragMessage of
      dmDragEnter:
      begin
        FDragDropProcessingFlag := True;
        OldDragOverTreeNode := nil;
        FDragObject := Source;
      end;
      dmDragMove:
      begin
        lpht.pt := ScreenToClient(Point(Pos.X, Pos.Y));
        lpht.flags := TVHT_ONITEM;
        TreeNode := Items.GetNode(TreeView_HitTest(Handle, lpht));

        if TreeNode <> nil then
        begin
          if (OldDragOverTreeNode <> nil) and (OldDragOverTreeNode = TreeNode) then
          begin
            if (TreeNode = TopItem) or
              ((TreeNode.GetPrevVisible <> nil) and (TreeNode.GetPrevVisible = TopItem))
              and (TopItem.GetPrevVisible <> nil) then
              TopItem := TopItem.GetPrevVisible
            else
              if ((TreeNode.GetNextVisible <> nil) and not IsNodeVisible(TreeNode.GetNextVisible)
                or ((TreeNode.GetNextVisible <> nil) and (TreeNode.GetNextVisible.GetNextVisible <> nil)
                and not IsNodeVisible(TreeNode.GetNextVisible.GetNextVisible))) then
                TopItem := TopItem.GetNextVisible;
          end;
          if OldDragOverTreeNode <> TreeNode then
          begin
            OldDragOverTreeNode := TreeNode;
            HideDragImage;
            TreeView_SelectDropTarget(Handle, OldDragOverTreeNode.ItemId);
            ShowDragImage;
          end;
        end
        else
        begin
          OldDragOverTreeNode := nil;
          HideDragImage;
          TreeView_SelectDropTarget(Handle, nil);
          ShowDragImage;
        end;
      end;
      dmDragLeave:
      begin
        if DropSourceTreeNode <> nil then
        begin
          HideDragImage;
          TreeView_SelectDropTarget(Handle, nil);
          DropSourceTreeNode.Selected := True;
          ShowDragImage;
        end;
        FDragObject := nil;
      end;
      dmDragCancel:
      begin
        FDragObject := nil;
        FDragDropProcessingFlag := False;
        OldDragOverTreeNode := nil;
        Invalidate;
      end;
      dmDragDrop:
      begin
        FDragObject := nil;
        IsCopyFlag := GetKeyState(VK_CONTROL) < 0;
        if OldDragOverTreeNode <> nil then
        begin
          TreeView_SelectDropTarget(Handle, nil);
          OldDragOverTreeNode.Selected := True;
          Invalidate;
          if (DropSourceTreeNode <> nil) and not OldDragOverTreeNode.HasAsParent(DropSourceTreeNode)
            and (OldDragOverTreeNode <> DropSourceTreeNode) then
          begin
            DoDragDropIsCopy(DropSourceTreeNode, OldDragOverTreeNode, IsCopyFlag);
            if ((OldDragOverTreeNode <> DropSourceTreeNode.Parent)
              or IsCopyFlag or Assigned(FOnEndDragTreeNode)) then
            begin
              CorrectDragFlag := True;
              DoDragDropTreeNode(OldDragOverTreeNode, DropSourceTreeNode, CorrectDragFlag);
            end;
          end;
        end
        else
          if DropSourceTreeNode <> nil then
          begin
            DoDragDropIsCopy(DropSourceTreeNode, OldDragOverTreeNode, IsCopyFlag);
            DoDragDropTreeNode(OldDragOverTreeNode, DropSourceTreeNode, CorrectDragFlag);
          end;
        if CorrectDragFlag then
        begin
          TreeNode := MoveTreeNodeStructure(DropSourceTreeNode, OldDragOverTreeNode, IsCopyFlag);
          if TreeNode <> nil then
          begin
            TreeNode.MakeVisible;
            Selected := TreeNode;
          end;
          Message.DragMessage := dmDragDrop;
          DropTarget := OldDragOverTreeNode;
        end;
        FDragDropProcessingFlag := False;
        OldDragOverTreeNode := nil;
        Invalidate;
      end;  // dmDragDrop:
    end;  // case
  end;  // with

  if (DropSourceTreeNode <> nil) and (Message.DragMessage <> dmDragDrop)
   and (Message.DragMessage <> dmDragCancel)
   and (OldDragOverTreeNode <> DropSourceTreeNode) then
  begin
    if OldDragOverTreeNode <> nil then
    begin
      CorrectDragFlag := (not DropSourceTreeNode.HasChildren
        or not OldDragOverTreeNode.HasAsParent(DropSourceTreeNode))
        and not Assigned(OnDragOver);
      if CorrectDragFlag then
        DoDragDropTreeNode(OldDragOverTreeNode, DropSourceTreeNode, CorrectDragFlag);
   end
   else
     DoDragDropTreeNode(OldDragOverTreeNode, DropSourceTreeNode, CorrectDragFlag);
 end;

 inherited;

 if (Message.DragMessage = dmDragDrop) or (Message.DragMessage = dmDragCancel) then
 begin
   DropSourceTreeNode := nil;
   dxTreeViewDragImages := nil;
   DropTarget := nil;
 end;

 if ((Message.DragMessage = dmDragMove) or (Message.DragMessage = dmDragLeave))
   and CorrectDragFlag and (Message.Result = 0) then
   Message.Result := 1;
end;

function TCustomdxTreeView.MoveTreeNodeStructure(Source, Destination: TTreeNode; IsCopy: Boolean): TTreeNode;
var
  i, ind: Integer;
  ListS, ListD: TList;
  tr: TTreeNode;
  AStyle: TNodeAttachMode;
begin
  Result := nil;
  ListS := TList.Create;
  ListD := TList.Create;
  CopyTreeNodeStructFlag := IsCopy or (Self <> Source.TreeView);
  GetNodeStructure(Source, ListS);
  CopyTreeNodeStructFlag := False;
  if Destination <> nil then
    AStyle := naAddChild
  else
    AStyle := naAdd;

  if Assigned(FOnEndDragTreeNode) then
    FOnEndDragTreeNode(Destination, Source, AStyle);

  if (AStyle = naAddChild) or (AStyle = naAddChildFirst) then
    Destination.HasChildren := True;
  if (Self <> Source.TreeView) or IsCopy then
  begin
    CopyTreeNodeStructFlag := True;
    tr := nil;
    for i := 0 to ListS.Count - 1 do
    begin
      if i = 0 then
        case AStyle of
          naAdd: tr := Items.Add(Destination, GetListItemText(TTreeNode(ListS.List[i])));
          naAddFirst: tr := Items.AddFirst(Destination, GetListItemText(TTreeNode(ListS.List[i])));
          naAddChild: tr := Items.AddChild(Destination, GetListItemText(TTreeNode(ListS.List[i])));
          naAddChildFirst: tr := Items.AddChildFirst(Destination, GetListItemText(TTreeNode(ListS.List[i])));
          naInsert: tr := Items.Insert(Destination, GetListItemText(TTreeNode(ListS.List[i])));
        end
      else
      begin
        ind := ListS.IndexOf(TTreeNode(ListS.List[i]).Parent);
        tr := Items.AddChild(TTreeNode(ListD.List[ind]), GetListItemText(TTreeNode(ListS.List[i])))
      end;
      if tr <> nil then
        ListD.Add(tr);
    end;
    InsertTreeNodeStructure(ListS, ListD);
    CopyTreeNodeStructFlag := False;
    if ListD.Count > 0 then
    begin
      Result := TTreeNode(ListD[0]);
      Destination.HasChildren := True;
    end;
  end;
  if (Self = Source.TreeView) and not IsCopy then
  begin
    CopyTreeNodeStructFlag := True;
    Source.MoveTo(Destination, AStyle);
    CopyTreeNodeStructFlag := False;
    Result := Source;
  end;
  ListS.Free;
  ListD.Free;
  if (Source <> nil) and not TdxTreeView(Source.TreeView).ReadOnly
    and (Self <> Source.TreeView) and not IsCopy then
    Source.Free;
end;

procedure TCustomdxTreeView.UpdateSizeGripCorner;
begin
  //do nothing
end;

function TCustomdxTreeView.GetNodeFromItem(const Item: TTVItem): TTreeNode;
begin
  with Item do
    if (state and TVIF_PARAM) <> 0 then
    begin
      Result := TTreeNode(lParam);
    end else Result := Items.GetNode(hItem);
end;

function TCustomdxTreeView.GetListItemText(TreeNode: TTreeNode): String;
begin
  if TreeNode <> nil then
    Result := TreeNode.Text;
end;

function TCustomdxTreeView.IsCustomDraw: Boolean;
begin
  Result := Assigned(FOnCustomDraw);
end;

procedure TCustomdxTreeView.DefineProperties(AFiler: TFiler);
begin
   inherited DefineProperties(AFiler);
   AFiler.DefineProperty('IsConfirmationAssigned', ReadIsAssigned,
     WriteIsAssigned, IsConfirmationAssigned);
end;

procedure TCustomdxTreeView.DoCustomDraw(TreeNode: TTreeNode; AFont: TFont;
  var AColor, ABkColor: TColor);
begin
  if Assigned(FOnCustomDraw) then
    FOnCustomDraw(Self, TreeNode, AFont, AColor, ABkColor);
end;

function TCustomdxTreeView.DoCNNotify(var Message: TWMNotify): Boolean;
var
  pnmlv: PNMTVCustomDraw;
  tr: TTreeNode;
  htr: HTReeItem;
  Color, BkColor: TColor;
  St: String;
begin
  Result := True;
  if csDestroying in ComponentState then
    Exit;

  if (Message.nmhdr^.code = NM_CUSTOMDRAW) and
    not (Assigned(OnAdvancedCustomDraw) or Assigned(OnAdvancedCustomDrawItem)) then
  begin
    Result := False;
    pnmlv := PNMTVCustomDraw(TMessage(Message).lParam);
    if pnmlv^.nmcd.dwDrawStage = CDDS_PREPAINT then
    begin
      if IsCustomDraw then
        Message.Result := CDRF_NOTIFYITEMDRAW
      else
        Message.Result := CDRF_DODEFAULT;
    end;
    if pnmlv^.nmcd.dwDrawStage = CDDS_ITEMPREPAINT then
    begin
      htr := Pointer(pnmlv^.nmcd.dwItemSpec);
      tr := Items.GetNode(htr);
      fntcd.Assign(Font);
      Color := Font.Color;
      BkColor := clWindow;
      DoCustomDraw( tr, fntcd, Color, BkColor);
      if ((pnmlv^.nmcd.uItemState and TVGN_CARET = 0)
        and (pnmlv^.nmcd.uItemState and TVGN_DROPHILITE = 0)
        and (OldDragOverTreeNode <> tr)) or not Focused then
      begin
        pnmlv^.clrText := ColorToRGB(Color);
        pnmlv^.clrTextBk := ColorToRGB(bkColor);
      end;

      SelectObject(pnmlv^.nmcd.hdc, fntcd.Handle);
      Message.Result := CDRF_NEWFONT;
    end;
  end;

  with Message.NMHdr^ do
    if code = TVN_BEGINLABELEDIT then
    begin
      SetImeMode(TreeView_GetEditControl(handle), ImeMode);
      if Assigned(FOnSetEditText) then
        with PTVDispInfo(Pointer(Message.NMHdr))^ do
        begin
          tr := GetNodeFromItem(item);
          St := tr.Text;
          FOnSetEditText(Self, tr, St);
          SendMessage(TreeView_GetEditControl(handle), WM_SETTEXT, 0, LPARAM(PChar(St)));
        end;
    end;
end;

procedure TCustomdxTreeView.CNNotify(var Message: TWMNotify);
begin
  if DoCNNotify(Message) then
    inherited;
end;

type
  PTNToolTipTextA = ^TTNToolTipTextA;
  TTNToolTipTextA = packed record
    hdr: TNMHDR;
    lpszText: PAnsiChar;
    szText: array[0..79] of AnsiChar;
    hinst: THandle;
    uFlags: UINT;
  end;

  PTNToolTipTextW = ^TTNToolTipTextW;
  TTNToolTipTextW = packed record
    hdr: TNMHDR;
    lpszText: PWideChar;
    szText: array[0..79] of WideChar;
    hinst: THandle;
    uFlags: UINT;
  end;

function TCustomdxTreeView.DoWMNotify(var Message: TWMNotify): Boolean;
begin
  Result := True;
  if not ShowNodeHint then
  begin
    case Message.nmhdr^.code of
      TTN_NEEDTEXTA, TTN_NEEDTEXTW:
        begin
          if Message.nmhdr^.code = TTN_NEEDTEXTW then
            PTNToolTipTextW(TMessage(Message).LParam).lpszText := ''
          else
            PTNToolTipTextA(TMessage(Message).LParam).lpszText := '';

          TMessage(Message).Result := 0;
          Result := False;
        end;
    end;
  end;
end;

procedure TCustomdxTreeView.WMNotify(var Message: TWMNotify);
begin
  if DoWMNotify(Message) then
    inherited;
end;

initialization
  OldDragOverTreeNode := nil;
  dxTreeViewDragImages := nil;
  Screen.Cursors[crdxTreeDrag] := LoadCursor(HInstance, 'DXTREEDRAGCOPY');

end.

