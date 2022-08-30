{$I TMSDEFS.INC}
{***********************************************************************}
{ TPlannerMonthView component                                           }
{ for Delphi & C++Builder                                               }
{                                                                       }
{ written by :                                                          }
{            TMS Software                                               }
{            copyright © 2005 - 2012                                    }
{            Email : info@tmssoftware.com                               }
{            Website : http://www.tmssoftware.com                       }
{                                                                       }
{ The source code is given as is. The author is not responsible         }
{ for any possible damage done due to the use of this code.             }
{ The component can be freely used in any application. The source       }
{ code remains property of the writer and may not be distributed        }
{ freely as such.                                                       }
{***********************************************************************}

unit PlannerMonthViewRegDE;



interface

uses
  Classes, SysUtils
  , PlannerMonthView, Planner, Dialogs, Forms, PlanStyles
   , AdvImage, AdvImgDe
{$IFDEF DELPHI6_LVL}
  , DesignIntf, DesignEditors

{$ELSE}
  , DsgnIntf
{$ENDIF}
  ;

type
  TPlannerMonthViewEditor = class(TDefaultEditor)
  protected
  {$IFNDEF DELPHI6_LVL}
    procedure EditProperty(PropertyEditor: TPropertyEditor;
                           var Continue, FreeEditor: Boolean); override;
  {$ELSE}
    procedure EditProperty(const PropertyEditor:IProperty; var Continue:Boolean); override;
  {$ENDIF}
  public
    function GetVerb(index:integer):string; override;
    function GetVerbCount:integer; override;
    procedure ExecuteVerb(Index:integer); override;
  end;

  TPMVSkinProperty = class(TClassProperty)
  public
    function GetAttributes:TPropertyAttributes; override;
    procedure Edit; override;
  end;


procedure Register;

implementation

uses
  Controls, AdvStyleIF;
  
procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TAdvImage), TPlannerMonthView, 'Background', TAdvImageProperty);
  RegisterPropertyEditor(TypeInfo(TPlannerSkin),TPlannerMonthView,'Skin',TPMVSkinProperty);
  RegisterComponentEditor(TPlannerMonthView, TPlannerMonthViewEditor);
end;


procedure TPlannerMonthViewEditor.ExecuteVerb(Index: integer);
var
  compiler:string;
  psf: TPlanStyleForm;
  style: TTMSStyle;
  s: TPlannerMonthViewStyle;

  function StyleToPlannerMonthViewStyle(AStyle: TTMSStyle): TPlannerMonthViewStyle;
  begin
    Result := pmsOffice2003Blue;
    case AStyle of
      tsOffice2003Blue: Result := pmsOffice2003Blue;
      tsOffice2003Silver: Result := pmsOffice2003Silver;
      tsOffice2003Olive: Result := pmsOffice2003Olive;
      tsOffice2003Classic: Result := pmsOffice2003Classic;
      tsOffice2007Luna: Result := pmsOffice2007Luna;
      tsOffice2007Obsidian: Result := pmsOffice2007Obsidian;
      tsWindowsXP: Result := pmsWindowsXP;
      tsWhidbey: Result := pmsWhidbey;
      tsCustom: Result := pmsCustom;
      tsOffice2007Silver: Result := pmsOffice2007Silver;
      tsWindowsVista: Result := pmsWindowsVista;
      tsWindows7: Result := pmsWindows7;
      tsTerminal: Result := pmsTerminal;
      tsOffice2010Blue: Result := pmsOffice2010Blue;
      tsOffice2010Silver: Result := pmsOffice2010Silver;
      tsOffice2010Black: Result := pmsOffice2010Black;
      tsWindows8: Result := pmsWindows8;
      tsOffice2013White: Result := pmsOffice2013White;
      tsOffice2013LightGray: Result := pmsOffice2013LightGray;
      tsOffice2013Gray: Result := pmsOffice2013Gray;
      tsWindows10: Result := pmsWindows10;
      tsOffice2016White: Result := pmsOffice2016White;
      tsOffice2016Gray: Result := pmsOffice2016Gray;
      tsOffice2016Black: Result := pmsOffice2016Black;
    end;
  end;
begin
  case Index of
  0:begin
      {$I COMPILERTEXT.INC}
      MessageDlg(Component.ClassName+' version '+ (Component as TPlannerMonthView).VersionString + ' for ' + compiler + #13#10#13#10'© 2004 - 2015 by TMS software'#13#10'http://www.tmssoftware.com',
                 mtInformation,[mbok],0);
    end;

  1: Edit;
  3: begin
       (Component as TPlannerMonthView).Skin.ClearSkin;
       Designer.Modified;
     end;
  2: begin
       style := (Component as TPlannerMonthView).GetComponentStyle;

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

         s := StyleToPlannerMonthViewStyle(style);
         (Component as TPlannerMonthView).TMSStyle := style;
         (Component as TPlannerMonthView).Style := s;
         Designer.Modified;
       end;
       psf.Free;
    end;
  end;
end;

{$IFNDEF DELPHI6_LVL}
procedure TPlannerMonthViewEditor.EditProperty(PropertyEditor: TPropertyEditor;
                                      var Continue, FreeEditor: Boolean);
{$ELSE}
procedure TPlannerMonthViewEditor.EditProperty(const PropertyEditor:IProperty;
                                      var Continue:Boolean);
{$ENDIF}
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


function TPlannerMonthViewEditor.GetVerb(Index: Integer): string;
begin
  Result := '';
  case Index of
  0:Result := 'About';
  1:Result := 'Items Editor';
  3:Result := 'Clear skin';
  2:Result := 'Styles';
  end;
end;

function TPlannerMonthViewEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;

{ TSkinProperty }

procedure TPMVSkinProperty.Edit;
var
  od: TOpenDialog;
  Planner: TPlannerMonthView;
begin
  Planner := TPlannerMonthView(Getcomponent(0));

  od := TOpenDialog.Create(Application);
  od.Filter := 'Planner Skin (*.plskin)|*.plskin|All files (*.*)|*.*';

  if od.Execute then
  begin
    Planner.Skin.LoadFromFile(od.FileName);
    Modified;
  end;

  od.Free;
end;

function TPMVSkinProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;



end.
