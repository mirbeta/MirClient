{***********************************************************************}
{ TAdvNavBar component                                                  }
{ for Delphi & C++ Builder                                              }
{                                                                       }
{ written by :                                                          }
{            TMS Software                                               }
{            copyright © 1999-2012                                      }
{            Email : info@tmssoftware.com                               }
{            Website : http://www.tmssoftware.com                       }
{                                                                       }
{ The source code is given as is. The author is not responsible         }
{ for any possible damage done due to the use of this code.             }
{ The component can be freely used in any application. The source       }
{ code remains property of the writer and may not be distributed        }
{ freely as such.                                                       }
{***********************************************************************}

unit AdvNavBarRegDE;

{$I TMSDEFS.INC}

interface

uses
  Classes, Controls, AdvNavBar, DesignIntf, DesignEditors;

type
  TAdvNavBarEditor = class(TDefaultEditor)
  protected
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TAdvNavBarPanelEditor = class(TDefaultEditor)
  protected
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

procedure Register;


implementation

procedure Register;
begin
  RegisterComponentEditor(TAdvNavBar,TAdvNavBarEditor);
  RegisterComponentEditor(TAdvNavBarPanel,TAdvNavBarPanelEditor);
end;


{ TAdvNavBarEditor }

procedure TAdvNavBarEditor.ExecuteVerb(Index: Integer);
var
  aAdvNavBarPanel: TAdvNavBarPanel;
begin
  inherited;
  case Index of
    0:
    begin
      TAdvNavBar(Component).ControlStyle := TAdvNavBar(Component).ControlStyle + [csAcceptsControls];
      aAdvNavBarPanel := TAdvNavBarPanel(Designer.CreateComponent(TAdvNavBarPanel,Component,0,0,100,100));
      aAdvNavBarPanel.Parent := TAdvNavBar(Component);

      aAdvNavBarPanel.AdvNavBar := TAdvNavBar(Component);
      (Component as TAdvNavBar).Invalidate;
      TCustomControl(Component).ControlStyle := TAdvNavBar(Component).ControlStyle - [csAcceptsControls];
      aAdvNavBarPanel.Caption := aAdvNavBarPanel.Name;
    end;
    1:
    begin
      TAdvNavBar(Component).SelectPreviousPanel;
    end;
    2:
    begin
      TAdvNavBar(Component).SelectNextPanel;
    end;
    3..18:
    begin
      TAdvNavBar(Component).Style := TAdvNavBarStyle(Index - 3);
      Designer.Modified;
    end;
  end;
end;

function TAdvNavBarEditor.GetVerb(Index: Integer): string;
begin
  case index of
    0: Result := 'Add panel';
    1: Result := 'Previous Panel';
    2: Result := 'Next Panel';
    3: Result := 'Outlook 2003 Blue look';
    4: Result := 'Outlook 2003 Silver look';
    5: Result := 'Outlook 2003 Olive look';
    6: Result := 'Outlook 2003 Classic look';
    7: Result := 'Outlook 2007 Luna look';
    8: Result := 'Outlook 2007 Obsidian look';
    9: Result := 'Windows XP look';
   10: Result := 'Whidbey look';
   11: Result := 'Custom look';
   12: Result := 'Office 2007 Silver look';
   13: Result := 'Windows Vista look';
   14: Result := 'Windows 7 look';
   15: Result := 'Terminal look';
   16: Result := 'Office 2010 Blue look';
   17: Result := 'Office 2010 Silver look';
   18: Result := 'Office 2010 Black look';
  end;
end;

function TAdvNavBarEditor.GetVerbCount: Integer;
begin
  Result := 19;
end;

{ TAdvNavBarPanelEditor }

procedure TAdvNavBarPanelEditor.ExecuteVerb(Index: Integer);
var
  aAdvNavBarPanel: TAdvNavBarPanel;
begin
  inherited;
  case Index of
    0:
    begin
      TAdvNavBarPanel(Component).Parent.ControlStyle := TAdvNavBarPanel(Component).Parent.ControlStyle + [csAcceptsControls];
      aAdvNavBarPanel := TAdvNavBarPanel(Designer.CreateComponent(TAdvNavBarPanel,TAdvNavBarPanel(Component).Parent,0,0,100,100));

      aAdvNavBarPanel.AdvNavBar := TAdvNavBar(TAdvNavBarPanel(Component).Parent);
      TAdvNavBar(TAdvNavBarPanel(Component).Parent).Invalidate;
      TAdvNavBarPanel(Component).Parent.ControlStyle := TAdvNavBarPanel(Component).Parent.ControlStyle - [csAcceptsControls];
      aAdvNavBarPanel.Caption := aAdvNavBarPanel.Name;
    end;
    1:
    begin
      TAdvNavBar(TAdvNavBarPanel(Component).Parent).SelectPreviousPanel;
    end;
    2:
    begin
      TAdvNavBar(TAdvNavBarPanel(Component).Parent).SelectNextPanel;
    end;
    3:
    begin
      TAdvNavBarPanel(Component).AdvNavBar:=  nil;
      Component.Free;
    end;
  end;
end;

function TAdvNavBarPanelEditor.GetVerb(Index: Integer): string;
begin
  case index of
    0: Result := 'Add panel';
    1: Result := 'Previous Panel';
    2: Result := 'Next Panel';
    3: Result := 'Delete Panel';
  end;
end;

function TAdvNavBarPanelEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;

end.
