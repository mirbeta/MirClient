{***********************************************************************}
{ TToolPanels component                                                 }
{ for Delphi & C++Builder                                               }
{ version 1.3                                                           }
{                                                                       }
{ written by TMS Software                                               }
{            copyright © 2003 - 2012                                    }
{            Email: info@tmssoftware.com                                }
{            Web: http://www.tmssoftware.com                            }
{                                                                       }
{ The source code is given as is. The author is not responsible         }
{ for any possible damage done due to the use of this code.             }
{ The component can be freely used in any application. The complete     }
{ source code remains property of the author and may not be distributed,}
{ published, given or sold in any form as such. No parts of the source  }
{ code can be included in any other component or application without    }
{ written authorization of the author.                                  }
{***********************************************************************}

unit ToolPanelsDE;

interface

{$I TMSDEFS.INC}
uses
  Classes, ToolPanels, Windows, Forms, TypInfo, Dialogs, ExtCtrls, Controls, SysUtils
{$IFDEF DELPHI6_LVL}
  , DesignIntf, DesignEditors, ContNrs
{$ELSE}
  , DsgnIntf
{$ENDIF}
  ;

type
  TAdvToolPanelTabEditor = class(TDefaultEditor)
  protected
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TAdvToolPanelEditor = class(TDefaultEditor)
  protected
    procedure EditProperty(const PropertyEditor:IProperty; var Continue:Boolean); override;
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

implementation

{ TToolPanelTabEditor }

procedure TAdvToolPanelTabEditor.ExecuteVerb(Index: integer);
begin
  inherited;
  case Index of
  0:
    begin
      TCustomPanel(Component).ControlStyle := TCustomPanel(Component).ControlStyle + [csAcceptsControls];
      Designer.CreateComponent(TAdvToolPanel,Component,23,0,100,100);

      with TAdvToolPanelTab(Component) do
        UpdatePanels(ControlCount - 1);

      (Component as TCustomPanel).Invalidate;
      TCustomPanel(Component).ControlStyle := TCustomPanel(Component).ControlStyle - [csAcceptsControls];
    end;
  1: TAdvToolPanelTab(Component).PrevPanel;
  2: TAdvToolPanelTab(Component).NextPanel;
  3..8:
    begin
      TAdvToolPanelTab(Component).Style := TToolPanelStyle(Index - 3);
      Designer.Modified;
    end;
  9:begin
      TAdvToolPanelTab(Component).Style := esOffice2007Silver;
      Designer.Modified;
    end;
  10:begin
      TAdvToolPanelTab(Component).Style := esWhidbey;
      Designer.Modified;
     end;
  end;
end;

function TAdvToolPanelTabEditor.GetVerb(Index: Integer): string;
begin
  case Index of
  0: Result := 'New Panel';
  1: Result := 'Previous Panel';
  2: Result := 'Next Panel';
  3: Result := 'Outlook 2003 Blue look';
  4: Result := 'Outlook 2003 Silver look';
  5: Result := 'Outlook 2003 Olive look';
  6: Result := 'Outlook 2003 Classic look';
  7: Result := 'Outlook 2007 Blue look';
  8: Result := 'Outlook 2007 Black look';
  9: Result := 'Outlook 2007 Silver look';
  10: Result := 'Whidbey look (VS.NET 2005)';
  end;
end;

function TAdvToolPanelTabEditor.GetVerbCount: Integer;
begin
  Result := 11; //3;
end;

{ TToolPanelEditor }

procedure TAdvToolPanelEditor.ExecuteVerb(Index: integer);
begin
  inherited;
  
  //if not ((Component as TAdvToolPanel).Parent is TAdvToolPanelTab) then
  //  Index := Index + 3;

  case Index of
  0:
    begin
      TCustomPanel(Component).Parent.ControlStyle := TCustomPanel(Component).Parent.ControlStyle + [csAcceptsControls];
      Designer.CreateComponent(TAdvToolPanel,TCustomPanel(Component).Parent,23,0,100,100);

      with TAdvToolPanelTab(TCustomPanel(Component).Parent) do
        UpdatePanels(ControlCount - 1);

      (TCustomPanel(Component).Parent as TCustomPanel).Invalidate;
      TCustomPanel(Component).Parent.ControlStyle := TCustomPanel(Component).Parent.ControlStyle - [csAcceptsControls];
    end;
  1: TAdvToolPanelTab(TCustomPanel(Component).Parent).PrevPanel;
  2: TAdvToolPanelTab(TCustomPanel(Component).Parent).NextPanel;
  3..8:
    begin
      TAdvToolPanelTab(TCustomPanel(Component).Parent).Style := TToolPanelStyle(Index - 3);
      Designer.Modified;
    end;
  9:begin
      TAdvToolPanelTab(TCustomPanel(Component).Parent).Style := esOffice2007Silver;
      Designer.Modified;
    end;
  10:begin
      TAdvToolPanelTab(TCustomPanel(Component).Parent).Style := esWhidbey;
      Designer.Modified;
     end;
  end;
end;

procedure TAdvToolPanelEditor.EditProperty(const PropertyEditor:IProperty;
                                      var Continue:Boolean);
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'SECTIONS') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;



function TAdvToolPanelEditor.GetVerb(Index: Integer): string;
begin
  if (Component as TAdvToolPanel).Parent is TAdvToolPanelTab then
  begin
    case Index of
    0: Result := 'New Panel';
    1: Result := 'Previous Panel';
    2: Result := 'Next Panel';
    3: Result := 'Outlook 2003 Blue look';
    4: Result := 'Outlook 2003 Silver look';
    5: Result := 'Outlook 2003 Olive look';
    6: Result := 'Outlook 2003 Classic look';
    7: Result := 'Outlook 2007 Blue look';
    8: Result := 'Outlook 2007 Black look';
    9: Result := 'Outlook 2007 Silver look';
    10: Result := 'Whidbey look';
    end;
  end;
  {
  else
  begin
    case Index of
    0: Result := 'Outlook 2003 Blue look';
    1: Result := 'Outlook 2003 Silver look';
    2: Result := 'Outlook 2003 Olive look';
    3: Result := 'Outlook 2003 Classic look';
    end;
  end;
  }
end;

function TAdvToolPanelEditor.GetVerbCount: Integer;
begin
  if (Component as TAdvToolPanel).Parent is TAdvToolPanelTab then
    Result := 11//3
  else
    Result := 0; //4
end;



end.
