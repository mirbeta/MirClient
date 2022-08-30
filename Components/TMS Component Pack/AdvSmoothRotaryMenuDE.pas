{**************************************************************************}
{ TAdvSmoothRotaryMenu DESIGN TIME EDITOR                                  }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright © 2013                                              }
{            Email : info@tmssoftware.com                                  }
{            Web : http://www.tmssoftware.com                              }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit AdvSmoothRotaryMenuDE;

interface

{$I TMSDEFS.INC}

uses
  Classes, AdvSmoothRotaryMenu, DesignIntf, DesignEditors;

type
  TAdvSmoothRotaryMenuEditor = class(TDefaultEditor)
  protected
    procedure EditProperty(const PropertyEditor: IProperty; var Continue: Boolean); override;
  public
    function GetVerb(index:integer):string; override;
    function GetVerbCount:integer; override;
    procedure ExecuteVerb(Index:integer); override;
  end;

  TPictureContainerTextProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc:TGetStrProc); override;
  end;


implementation

uses
  SysUtils, Forms, Windows, AdvSmoothStyles, Dialogs, Controls, AdvStyleIF;

procedure TAdvSmoothRotaryMenuEditor.EditProperty(
  const PropertyEditor: IProperty; var Continue: Boolean);
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'ITEMS') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;

procedure TAdvSmoothRotaryMenuEditor.ExecuteVerb(Index: integer);
var
  psf: TAdvSmoothStyleForm;
  style: TTMSStyle;
  l, t: Integer;
  p: TPoint;
begin
  inherited;
  case Index of
  0:begin
      if (Component is TAdvSmoothRotaryMenuDialog) then
      begin
        l := LongRec(Component.DesignInfo).Lo;
        t := LongRec(Component.DesignInfo).Hi;
        p := Point(l, t);

        if (Component.Owner is TControl) then
          if Assigned((Component.Owner as TControl).Parent) then
            p := (Component.Owner as TControl).Parent.ClientToScreen(p);

        if (Component is  TAdvSmoothRotaryMenuDialog) then
         (Component as TAdvSmoothRotaryMenuDialog).PopupMenuAt(p.X, p.Y);
      end;
    end;
  1:begin
    style := tsOffice2003Blue;
    if (Component is TAdvSmoothRotaryMenu) then
      style := (Component as TAdvSmoothRotaryMenu).GetComponentStyle
    else if Component is TAdvSmoothRotaryMenuDialog then
      style := (Component as TAdvSmoothRotaryMenuDialog).GetComponentStyle;

    psf := TAdvSmoothStyleForm.Create(Application);
    case style of
      tsOffice2003Blue: psf.RadioGroup1.ItemIndex := 0;
      tsOffice2003Olive: psf.RadioGroup1.ItemIndex := 1;
      tsOffice2003Silver: psf.RadioGroup1.ItemIndex := 2;
      tsOffice2003Classic: psf.RadioGroup1.ItemIndex := 3;
      tsOffice2007Luna: psf.RadioGroup1.ItemIndex := 4;
      tsOffice2007Obsidian: psf.RadioGroup1.ItemIndex := 5;
      tsOffice2007Silver: psf.RadioGroup1.ItemIndex := 6;
      tsOffice2010Blue: psf.RadioGroup1.ItemIndex := 7;
      tsOffice2010Silver: psf.RadioGroup1.ItemIndex := 8;
      tsOffice2010Black: psf.RadioGroup1.ItemIndex := 9;
      tsWindowsXP: psf.RadioGroup1.ItemIndex := 10;
      tsWindowsVista: psf.RadioGroup1.ItemIndex := 11;
      tsWindows7: psf.RadioGroup1.ItemIndex := 12;
      tsTerminal: psf.RadioGroup1.ItemIndex := 13;
      tsWindows8: psf.RadioGroup1.ItemIndex := 14;
      tsOffice2013White: psf.RadioGroup1.ItemIndex := 15;
      tsOffice2013LightGray: psf.RadioGroup1.ItemIndex := 16;
      tsOffice2013Gray: psf.RadioGroup1.ItemIndex := 17;
      tsWindows10: psf.RadioGroup1.ItemIndex := 18;
      tsOffice2016White: psf.RadioGroup1.ItemIndex := 19;
      tsOffice2016Gray: psf.RadioGroup1.ItemIndex := 20;
      tsOffice2016Black: psf.RadioGroup1.ItemIndex := 21;
    end;

    if psf.ShowModal = mrOK then
    begin
      case psf.RadioGroup1.ItemIndex of
        0: style := tsOffice2003Blue;
        1: style := tsOffice2003Olive;
        2: style := tsOffice2003Silver;
        3: style := tsOffice2003Classic;
        4: style := tsOffice2007Luna;
        5: style := tsOffice2007Obsidian;
        6: style := tsOffice2007Silver;
        7: style := tsOffice2010Blue;
        8: style := tsOffice2010Silver;
        9: style := tsOffice2010Black;
        10: style := tsWindowsXP;
        11: style := tsWindowsVista;
        12: style := tsWindows7;
        13: style := tsTerminal;
        14: style := tsWindows8;
        15: style := tsOffice2013White;
        16: style := tsOffice2013LightGray;
        17: style := tsOffice2013Gray;
        18: style := tsWindows10;
        19: style := tsOffice2016White;
        20: style := tsOffice2016Gray;
        21: style := tsOffice2016Black;
      end;

        if (Component is TAdvSmoothRotaryMenu) then
          (Component as TAdvSmoothRotaryMenu).SetComponentStyle(style)
        else if Component is TAdvSmoothRotaryMenuDialog then
          (Component as TAdvSmoothRotaryMenuDialog).SetComponentStyle(style);

         Designer.Modified;
      end;
      psf.Free;
    end;
    2:
    begin
        if (Component is TAdvSmoothRotaryMenu) then
          (Component as TAdvSmoothRotaryMenu).SetDefaultStyle
        else if Component is TAdvSmoothRotaryMenuDialog then
          (Component as TAdvSmoothRotaryMenuDialog).Menu.SetDefaultStyle;

      Designer.Modified;
    end;
  end;
end;

function TAdvSmoothRotaryMenuEditor.GetVerb(index: integer): string;
begin
  case Index of
  0: Result := 'Preview';
  1: Result := 'Styles';
  2: Result := 'Default Style';
  end;
end;

function TAdvSmoothRotaryMenuEditor.GetVerbCount: integer;
begin
  Result := 3;
end;


{ TPictureContainerTextProperty }

function TPictureContainerTextProperty.GetAttributes: TPropertyAttributes;
begin
  {$IFDEF DELPHI2006_LVL}
  Result := [paValueList, paSortList, paValueEditable];
  {$ELSE}
  Result := [paValueList, paSortList];
  {$ENDIF}
end;

procedure TPictureContainerTextProperty.GetValues(Proc: TGetStrProc);
var
  comp: TPersistent;
  i: integer;
begin
  comp := GetComponent(0);

  if Assigned(comp) and (comp is TRotaryMenuItem) then
    comp := (comp as TRotaryMenuItem).GetMenu;

  if Assigned(comp) and (comp is TAdvSmoothRotaryMenu) then
  begin
    if Assigned((comp as TAdvSmoothRotaryMenu).PictureContainer) then
    begin
      for i := 0 to (comp as TAdvSmoothRotaryMenu).PictureContainer.Items.Count - 1 do
      begin
        Proc((comp as TAdvSmoothRotaryMenu).PictureContainer.Items.Items[i].Name);
      end;
    end;
  end;
end;



end.







