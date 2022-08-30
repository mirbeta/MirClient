{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library classes                   }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
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

unit dxComCtrlsUtils;

{$I cxVer.inc}

interface

uses
  Messages, Classes, Forms, Types, CommCtrl, ComCtrls, Controls,
  dxCore, cxControls;

function CustomizationTreeViewWndProcHandler(ATreeView: TTreeView; var Message: TMessage): Boolean;
function cxTreeViewFindNodeByData(ATreeView: TTreeView; AData: Pointer; var ANode: TTreeNode): Boolean;
function cxTreeViewGetHitNode(ATreeView: TTreeView; const ACursorPos: TPoint): TTreeNode;
procedure cxTreeViewGetCollapsed(ATreeView: TTreeView; AList: TList);
procedure cxTreeViewGetData(ATreeView: TTreeView; AList: TList);
procedure cxTreeViewGetSelection(ATreeView: TTreeView; AList: TList);
procedure cxTreeViewSetSelection(ATreeView: TTreeView; AList: TList);

implementation

function CustomizationTreeViewWndProcHandler(ATreeView: TTreeView; var Message: TMessage): Boolean;

  procedure ShowContextMenu;
  var
    AHitTest: THitTests;
  begin
    AHitTest := ATreeView.GetHitTestInfoAt(Message.LParamLo, Message.LParamHi);
    if (htOnItem in AHitTest) then
      ATreeView.Perform(WM_CONTEXTMENU, ATreeView.Handle, dxPointToLParam(GetMouseCursorPos));
  end;

var
  ANode: TTreeNode;
  AShift: TShiftState;
begin
  Result := False;
  case Message.Msg of
    CN_NOTIFY:
      case TWMNotify(Message).NMHdr^.code of
        NM_RCLICK:
          begin
            ShowContextMenu;
            Message.Result := 1;
            Result := True;
          end;
        end;
    WM_RBUTTONDOWN:
      begin
        if ATreeView.Selected <> nil then
          ATreeView.Selected.EndEdit(False);
        ANode := ATreeView.GetNodeAt(Message.LParamLo, Message.LParamHi);
        if ANode <> nil then
        begin
          ANode.Focused := True;
          AShift := KeysToShiftState(Message.WParam);
          if not ANode.Selected then
          begin
            if [ssShift, ssCtrl] * AShift <> [] then
              AShift := [];
            ATreeView.Select(ANode, AShift);
          end;
        end;
        Message.Result := 1;
        Result := True;
      end;
    WM_LBUTTONDOWN:
      begin
        ANode := ATreeView.GetNodeAt(Message.LParamLo, Message.LParamHi);
        if (ANode <> nil) then
        begin
          AShift := KeysToShiftState(Message.WParam);
          ATreeView.Select(ANode, AShift);
          ANode.Focused := True;
        end;
      end;
  end;
end;

function cxTreeViewFindNodeByData(ATreeView: TTreeView; AData: Pointer; var ANode: TTreeNode): Boolean;

  function FindNode(ANode: TTreeNode): TTreeNode;
  var
    I: Integer;
  begin
    Result := nil;
    if ANode.Data = AData then
      Result := ANode
    else
      if ANode.HasChildren then
        for I := 0 to ANode.Count - 1 do
        begin
          Result := FindNode(ANode.Item[I]);
          if Result <> nil then
            Break;
        end;
  end;

var
  I: Integer;
begin
  Result := False;
  for I := 0 to ATreeView.Items.Count - 1 do
  begin
    ANode := FindNode(ATreeView.Items[I]);
    if ANode <> nil then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function cxTreeViewGetHitNode(ATreeView: TTreeView; const ACursorPos: TPoint): TTreeNode;
begin
  if htOnItem in ATreeView.GetHitTestInfoAt(ACursorPos.X, ACursorPos.Y) then
    Result := ATreeView.GetNodeAt(ACursorPos.X, ACursorPos.Y)
  else
    Result := nil;
end;

procedure cxTreeViewGetCollapsed(ATreeView: TTreeView; AList: TList);

  procedure CheckNode(ANode: TTreeNode);
  var
    I: Integer;
  begin
    if ANode.HasChildren then
    begin
      if not ANode.Expanded then
        AList.Add(ANode.Data);
      for I := 0 to ANode.Count - 1 do
        CheckNode(ANode[I]);
    end;
  end;

var
  I: Integer;
begin
  AList.Clear;
  for I := 0 to ATreeView.Items.Count - 1 do
    CheckNode(ATreeView.Items[I]);
end;

procedure cxTreeViewGetData(ATreeView: TTreeView; AList: TList);

  procedure GetData(ANode: TTreeNode);
  var
    I: Integer;
  begin
    if ANode = nil then
      Exit;
    if (ANode.Data <> nil) and (AList.IndexOf(ANode.Data) = -1) then
      AList.Add(ANode.Data);
    for I := 0 to ANode.Count - 1 do
      GetData(ANode[I]);
  end;

var
  I: Integer;
begin
  AList.Clear;
  for I := 0 to ATreeView.Items.Count - 1 do
    GetData(ATreeView.Items[I])
end;

procedure cxTreeViewGetSelection(ATreeView: TTreeView; AList: TList);
var
  I: Integer;
begin
  for I := 0 to ATreeView.SelectionCount - 1 do
    AList.Add(ATreeView.Selections[I].Data);
end;

procedure cxTreeViewSetSelection(ATreeView: TTreeView; AList: TList);

  procedure InternalSetSelection(ANode: TTreeNode);
  var
    I: Integer;
  begin
    if ANode = nil then
      Exit;
    if ANode.Selected xor (ANode.Data <> nil) and (AList.IndexOf(ANode.Data) >= 0) then
      if ANode.Selected then
        ATreeView.Deselect(ANode)
      else
        ATreeView.Select(ANode, [ssCtrl]);
    ANode.Focused := ANode.Selected;
    for I := 0 to ANode.Count - 1 do
      InternalSetSelection(ANode[I]);
  end;

var
  I: Integer;
begin
  with ATreeView do
  begin
    if AList.Count > 0 then
      for I := 0 to Items.Count - 1 do
        InternalSetSelection(Items[I])
    else
      ATreeView.ClearSelection(False);
  end;
  if ATreeView.SelectionCount = 0 then
    ATreeView.Selected := nil;
end;

end.
