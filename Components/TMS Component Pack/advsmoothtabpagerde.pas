unit AdvSmoothTabPagerDE;

interface

{$I TMSDEFS.INC}

uses
  Classes, Graphics, Comctrls, Windows, Forms, TypInfo, Dialogs, ExtCtrls,
  Controls, ExtDlgs, AdvSmoothTabPager
{$IFDEF DELPHI6_LVL}
  , DesignIntf, DesignEditors, ContNrs
{$ELSE}
  , DsgnIntf
{$ENDIF}
  ;

type

  TAdvSmoothTabPagerEditor = class(TDefaultEditor)
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TAdvSmoothTabPageEditor = class(TDefaultEditor)
  public
    function GetVerb(Index: Integer):string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

implementation

uses
  SysUtils, AdvSmoothStyles, AdvStyleIF;

function HTMLToRgb(color: tcolor): tcolor;
var
  r,g,b: integer;
begin
  r := (Color and $0000FF);
  g := (Color and $00FF00);
  b := (Color and $FF0000) shr 16;
  Result := b or g or (r shl 16);
end;

function BrightnessColor(Col: TColor; Brightness: integer): TColor; overload;
var
  r1,g1,b1: Integer;
begin
  Col := Longint(ColorToRGB(Col));
  r1 := GetRValue(Col);
  g1 := GetGValue(Col);
  b1 := GetBValue(Col);

  r1 := Round( (100 + Brightness)/100 * r1 );
  g1 := Round( (100 + Brightness)/100 * g1 );
  b1 := Round( (100 + Brightness)/100 * b1 );

  Result := RGB(r1,g1,b1);
end;


function BrightnessColor(Col: TColor; BR,BG,BB: integer): TColor; overload;
var
  r1,g1,b1: Integer;
begin
  Col := Longint(ColorToRGB(Col));
  r1 := GetRValue(Col);
  g1 := GetGValue(Col);
  b1 := GetBValue(Col);

  r1 := Round( (100 + BR)/100 * r1 );
  g1 := Round( (100 + BG)/100 * g1 );
  b1 := Round( (100 + BB)/100 * b1 );

  Result := RGB(r1,g1,b1);
end;


{ TAdvToolBarPagerEditor }

procedure TAdvSmoothTabPagerEditor.ExecuteVerb(Index: Integer);
var
  AdvPage : TAdvSmoothTabPage;
  psf: TAdvSmoothStyleForm;
  style: TTMSStyle;
  i: integer;
begin
  inherited;
  if (Index = 0) then
  begin
    psf := TAdvSmoothStyleForm.Create(Application);
    if psf.ShowModal = mrOK then
    begin
      if (Component is TAdvSmoothTabPager) then
      begin
        style := tsOffice2003Blue;
        case psf.RadioGroup1.ItemIndex of
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
        end;
        (Component as TAdvSmoothTabPager).SetComponentStyle(style);
        for I := 0 to (Component as TAdvSmoothTabPager).AdvSmoothTabPageCount - 1 do
          (Component as TAdvSmoothTabPager).AdvSmoothTabPages[I].SetComponentStyle(style);
          
        Designer.Modified;
      end;
    end;
    psf.Free;
  end;
  case Index of
  1:
    begin
      AdvPage := TAdvSmoothTabPage(Designer.CreateComponent(TAdvSmoothTabPage,Component,23,0,100,100));
      AdvPage.Parent := TAdvSmoothTabPager(Component);
      AdvPage.AdvSmoothTabPager := TAdvSmoothTabPager(Component);
      AdvPage.Caption := AdvPage.name;
      TAdvSmoothTabpager(component).ActivePage:= AdvPage;
    end;
  2: TAdvSmoothTabPager(Component).SelectNextPage(false);
  3: TAdvSmoothTabPager(Component).SelectNextPage(True);
  end;
end;

function TAdvSmoothTabPagerEditor.GetVerb(Index: Integer): string;
begin
  case Index of
  0: Result := 'Styles';
  1: Result := 'New Page';
  2: Result := 'Previous Page';
  3: Result := 'Next Page';
  end;
end;

function TAdvSmoothTabPagerEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

{ TAdvPageEditor }

procedure TAdvSmoothTabPageEditor.ExecuteVerb(Index: Integer);
var
  AdvPage : TAdvSmoothTabPage;
  psf: TAdvSmoothStyleForm;
  style: TTMSStyle;
begin
  inherited;
  if (Index = 0) then
  begin
    style := (Component as TAdvSmoothTabPage).GetComponentStyle;

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
      if (Component is TAdvSmoothTabPage) then
         (Component as TAdvSmoothTabPage).SetComponentStyle(style);
         Designer.Modified;
    end;
    psf.Free;
  end;

  case Index of
  1:
    begin
      AdvPage := TAdvSmoothTabPage(Designer.CreateComponent(TAdvSmoothTabPage,TWinControl(Component).Parent,23,0,100,100));
      AdvPage.Parent := TWinControl(Component).Parent;
      AdvPage.AdvSmoothTabPager := TAdvSmoothTabPager(TWinControl(Component).Parent);
      AdvPage.Caption := AdvPage.Name;
      TAdvSmoothTabPager(TWinControl(Component).Parent).ActivePage:= AdvPage;
    end;
  2: TAdvSmoothTabPager(TAdvSmoothTabPage(Component).Parent).SelectNextPage(false);
  3: TAdvSmoothTabPager(TAdvSmoothTabPage(Component).Parent).SelectNextPage(true);
  4:
    begin
    TAdvSmoothTabPage(Component).AdvSmoothTabPager := nil;
    Component.Free;
    end;
  end;
end;

function TAdvSmoothTabPageEditor.GetVerb(Index: Integer): string;
begin
  case Index of
  0: Result := 'Styles';
  1: Result := 'New Page';
  2: Result := 'Previous Page';
  3: Result := 'Next Page';
  4: Result := 'Delete Page';
  end;
end;

function TAdvSmoothTabPageEditor.GetVerbCount: Integer;
begin
  Result := 5;
end;



end.
