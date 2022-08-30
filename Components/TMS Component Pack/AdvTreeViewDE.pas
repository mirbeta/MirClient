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

unit AdvTreeViewDE;

interface

{$I TMSDEFS.INC}

uses
  Classes, AdvTreeView, AdvCustomTreeView, AdvTreeViewData
  {$IFDEF FMXLIB}
  ,FMX.Dialogs
  {$ENDIF}
  {$IFDEF VCLLIB}
  ,Dialogs
  {$ENDIF}
  {$IFDEF WIN32}
  ,SysUtils, DesignIntf, DesignEditors
  {$ENDIF}
  ;

{$IFDEF WIN32}
type
  TAdvCustomTreeViewProtected = class(TAdvCustomTreeView);

  TAdvTreeViewEditor = class(TDefaultEditor)
  protected
    procedure EditProperty(const PropertyEditor: IProperty; var Continue: Boolean); override;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;
{$ENDIF}

implementation

{$IFDEF WIN32}
procedure TAdvTreeViewEditor.EditProperty(const PropertyEditor: IProperty; var Continue: Boolean);
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'COLUMNS') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;
{$ENDIF}

procedure TAdvTreeViewEditor.ExecuteVerb(Index: Integer);
var
  v: string;
  {$IFDEF DELPHIXE2_LVL}
  i: Integer;
  n: TAdvTreeViewNode;
  a: array of string;
  vs: array of string;
  {$ENDIF}
begin
  case Index of
    0: TAdvCustomTreeView(Component).ClearNodes;
    1: TAdvCustomTreeView(Component).ClearColumns;
    2:
    begin
      if InputQuery('Enter column text', 'Text:', v) then
        TAdvCustomTreeViewProtected(Component).Columns.Add.Text := v;
    end;
    {$IFDEF DELPHIXE2_LVL}
    3:
    begin
      SetLength(a, TAdvCustomTreeViewProtected(Component).Columns.Count);
      if Length(a) = 0 then
        raise Exception.Create('Please add a new column first')
      else
      begin
        for I := 0 to TAdvCustomTreeViewProtected(Component).Columns.Count - 1 do
          a[I] := 'Value for ' + TAdvCustomTreeViewProtected(Component).GetColumnText(I) + ':';

        SetLength(vs, Length(a));
        if InputQuery('Enter node text', a, vs) then
        begin
          n := TAdvCustomTreeViewProtected(Component).AddNode;
          for I := 0 to Length(vs) - 1 do
            n.Text[I] := vs[I];
        end;
      end;
    end;
    {$ENDIF}
  end;
end;

function TAdvTreeViewEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Clear &Nodes';
    1: Result := 'Clear &Columns';
    2: Result := '&Add Column';
    {$IFDEF DELPHIXE2_LVL}
    3: Result := 'Add N&ode';
    {$ENDIF}
  end;
end;

function TAdvTreeViewEditor.GetVerbCount: Integer;
begin
  {$IFDEF DELPHIXE2_LVL}
  Result := 4;
  {$ELSE}
  Result := 3;
  {$ENDIF}
end;

end.


