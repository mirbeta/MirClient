{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2015                                        }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The complete source code remains property of the author and may    }
{ not be distributed, published, given or sold in any form as such.  }
{ No parts of the source code can be included in any other component }
{ or application without written authorization of the author.        }
{********************************************************************}

unit AdvCheckedTreeView;

interface

{$I TMSDEFS.INC}

uses
  Classes, AdvTreeView, AdvTreeViewData
  {$IFDEF FMXLIB}
  ,AdvBaseControl
  {$ENDIF}
  ;

type
  TAdvCheckedTreeViewNodes = class;

  TAdvCheckedTreeViewNodeValue = class(TAdvTreeViewNodeValue)
  public
    constructor Create(Collection: TCollection); override;
  end;

  TAdvCheckedTreeViewNodeValues = class(TAdvTreeViewNodeValues)
  protected
    function GetItemClass: TCollectionItemClass; override;
  end;

  TAdvCheckedTreeViewNode = class(TAdvTreeViewNode)
  protected
    function CreateNodeValues: TAdvTreeViewNodeValues; override;
    function CreateNodes: TAdvTreeViewNodes; override;
  end;

  TAdvCheckedTreeViewNodes = class(TAdvTreeViewNodes)
  protected
    function GetItemClass: TCollectionItemClass; override;
  end;

  {$IFDEF FMXLIB}
  [ComponentPlatformsAttribute(TMSPlatforms)]
  {$ENDIF}
  {$IFDEF VCLLIB}
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  {$ENDIF}
  TAdvCheckedTreeView = class(TAdvTreeView)
  private
    function GetChecked(ANode: TAdvTreeViewNode): Boolean;
    procedure SetChecked(ANode: TAdvTreeViewNode; const Value: Boolean);
  protected
    function CreateNodes: TAdvTreeViewNodes; override;
  public
    procedure InitSample; override;
    property Checked[ANode: TAdvTreeViewNode]: Boolean read GetChecked write SetChecked;
  end;

implementation

{ TAdvCheckedTreeView }

function TAdvCheckedTreeView.CreateNodes: TAdvTreeViewNodes;
begin
  Result := TAdvCheckedTreeViewNodes.Create(Self, nil);
end;

function TAdvCheckedTreeView.GetChecked(ANode: TAdvTreeViewNode): Boolean;
begin
  Result := False;
  if Assigned(ANode) then
    Result := ANode.Checked[0];
end;

procedure TAdvCheckedTreeView.InitSample;
var
  pAudi, pMercedes, pSub, n: TAdvTreeViewNode;
begin
  BeginUpdate(True);
  Columns.Clear;
  Nodes.Clear;

  Columns.Add.Text := 'Model';

  pAudi := AddNode;
  pAudi.Text[0] := 'Audi';
  pAudi.Extended := True;

  pSub := AddNode(pAudi);
  pSub.Text[0] := 'A3';

  pSub := AddNode(pAudi);
  pSub.Text[0] := 'A5 series';

  n := AddNode(pSub);
  n.Text[0] := 'S5';

  n := AddNode(pSub);
  n.Text[0] := 'RS5';

  pSub := AddNode(pAudi);
  pSub.Text[0] := 'A8';

  pMercedes := AddNode;
  pMercedes.Text[0] := 'Mercedes';
  pMercedes.Extended := True;

  pSub := AddNode(pMercedes);
  pSub.Text[0] := 'SLS';

  pSub := AddNode(pMercedes);
  pSub.Text[0] := 'SLK';

  pSub := AddNode(pMercedes);
  pSub.Text[0] := 'GLA';

  ExpandAll;
  EndUpdate;
end;

procedure TAdvCheckedTreeView.SetChecked(ANode: TAdvTreeViewNode;
  const Value: Boolean);
begin
  if Assigned(ANode) then
    ANode.Checked[0] := Value;
end;

{ TAdvCheckedTreeViewNode }

function TAdvCheckedTreeViewNode.CreateNodes: TAdvTreeViewNodes;
begin
  Result := TAdvCheckedTreeViewNodes.Create(TreeView, Self);
end;

function TAdvCheckedTreeViewNode.CreateNodeValues: TAdvTreeViewNodeValues;
begin
  Result := TAdvCheckedTreeViewNodeValues.Create(TreeView, Self);
end;

{ TAdvCheckedTreeViewNodes }

function TAdvCheckedTreeViewNodes.GetItemClass: TCollectionItemClass;
begin
  Result := TAdvCheckedTreeViewNode;
end;

{ TAdvCheckedTreeViewNodeValue }

constructor TAdvCheckedTreeViewNodeValue.Create(Collection: TCollection);
begin
  inherited;
  CheckType := tvntCheckBox;
end;

{ TAdvCheckedTreeViewNodeValues }

function TAdvCheckedTreeViewNodeValues.GetItemClass: TCollectionItemClass;
begin
  Result := TAdvCheckedTreeViewNodeValue;
end;

end.

