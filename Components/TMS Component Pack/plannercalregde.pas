{***********************************************************************}
{ TPLANNERCALENDAR component                                            }
{ for Delphi & C++Builder                                               }
{                                                                       }
{ written by :                                                          }
{            TMS Software                                               }
{            copyright © 1999-2014                                      }
{            Email : info@tmssoftware.com                               }
{            Website : http://www.tmssoftware.com                       }
{***********************************************************************}
unit PlannerCalRegDE;

interface
{$I TMSDEFS.INC}
uses
  PlannerCal, PlannerDatePicker, PlannerMaskDatePicker, Classes, PlanStyles,
  AdvImage, AdvImgDE, DesignIntf, DesignEditors;

type
  TPlannerCalendarEditProperty = class(TClassProperty);

  TPlannerCalendarEditor = class(TDefaultEditor)
  protected
  public
    function GetVerb(index:integer):string; override;
    function GetVerbCount:integer; override;
    procedure ExecuteVerb(Index:integer); override;
  end;


procedure Register;

implementation

uses
  Dialogs, Forms, Controls, AdvStyleIF;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TAdvImage), TPlannerCalendar, 'Background', TAdvImageProperty);
  RegisterPropertyEditor(TypeInfo(TAdvImage), TPlannerCalendarGroup, 'Background', TAdvImageProperty);
  RegisterPropertyEditor(TypeInfo(TPlannerCalendar), TPlannerDatePicker, 'Calendar', TPlannerCalendarEditProperty);
  RegisterPropertyEditor(TypeInfo(TPlannerCalendar), TPlannerMaskDatePicker, 'Calendar', TPlannerCalendarEditProperty);

  RegisterComponentEditor(TPlannerCalendar, TPlannerCalendarEditor);
  RegisterComponentEditor(TPlannerDatePicker, TPlannerCalendarEditor);
  RegisterComponentEditor(TPlannerCalendarGroup, TPlannerCalendarEditor);  
end;



{ TPlannerCalendarEditor }

procedure TPlannerCalendarEditor.ExecuteVerb(Index: integer);
var
  psf: TPlanStyleForm;
  style: TTMSStyle;
  s: TPlannerCalendarStyle;
  function StyleToPlannerStyle(AStyle: TTMSStyle): TPlannerCalendarStyle;
  begin
    Result := psOffice2003Blue;
    case AStyle of
      tsOffice2003Blue: Result := psOffice2003Blue;
      tsOffice2003Silver: Result := psOffice2003Silver;
      tsOffice2003Olive: Result := psOffice2003Olive;
      tsOffice2003Classic: Result := psOffice2003Classic;
      tsOffice2007Luna: Result := psOffice2007Luna;
      tsOffice2007Obsidian: Result := psOffice2007Obsidian;
      tsWindowsXP: Result := psWindowsXP;
      tsWhidbey: Result := psWhidbey;
      tsCustom: Result := psCustom;
      tsOffice2007Silver: Result := psOffice2007Silver;
      tsWindowsVista: Result := psWindowsVista;
      tsWindows7: Result := psWindows7;
      tsTerminal: Result := psTerminal;
      tsOffice2010Blue: Result := psOffice2010Blue;
      tsOffice2010Silver: Result := psOffice2010Silver;
      tsOffice2010Black: Result := psOffice2010Black;
      tsWindows8: Result := psWindows8;
      tsOffice2013White: Result := psOffice2013White;
      tsOffice2013LightGray: Result := psOffice2013LightGray;
      tsOffice2013Gray: Result := psOffice2013Gray;
      tsWindows10: Result := psWindows10;
      tsOffice2016White: Result := psOffice2016White;
      tsOffice2016Gray: Result := psOffice2016Gray;
      tsOffice2016Black: Result := psOffice2016Black;
    end;
  end;
begin
  inherited;
  if Index = 0 then
  begin
    style := tsOffice2003Blue;
    if (Component is TPlannerCalendar) then
      style := (Component as TPlannerCalendar).GetComponentStyle;
    if (Component is TPlannerDatePicker) then
      style := (Component as TPlannerDatePicker).GetComponentStyle;
    if (Component is TPlannerCalendarGroup) then
      style := (Component as TPlannerCalendarGroup).GetComponentStyle;

    psf := TPlanStyleForm.Create(Application);
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

      s := StyleToPlannerStyle(style);

      if (Component is TPlannerCalendar) then
      begin
        (Component as TPlannerCalendar).Style := s;
        (Component as TPlannerCalendar).TMSStyle := style;
      end;
      if (Component is TPlannerDatePicker) then
      begin
        (Component as TPlannerDatePicker).Calendar.Style := s;
        (Component as TPlannerDatePicker).Calendar.TMSStyle := style;
      end;
      if (Component is TPlannerCalendarGroup) then
      begin
        (Component as TPlannerCalendarGroup).Style := s;
        (Component as TPlannerCalendarGroup).TMSStyle := style;
      end;

      Designer.Modified;
    end;
    psf.Free;
  end;
end;

function TPlannerCalendarEditor.GetVerb(index: integer): string;
begin
  if Index = 0 then
    Result := 'Styles';
end;

function TPlannerCalendarEditor.GetVerbCount: integer;
begin
  Result := 1;
end;

end.


