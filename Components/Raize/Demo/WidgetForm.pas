{=======================================================================================================================
  WidgetForm Unit

  Raize Components - Demo Program Source Unit

  Copyright © 1995-2002 by Raize Software, Inc.  All Rights Reserved.
=======================================================================================================================}

{$I RCDemo.inc}

unit WidgetForm;

interface

uses
  Forms,
  RzPopups,
  Controls,
  ExtCtrls,
  RzPanel,
  RzBorder,
  Graphics,
  Classes,
  RzForms;

type
  TFrmWidgets = class(TForm)
    RzCalendar1: TRzCalendar;
    RzTimePicker1: TRzTimePicker;
    RzColorPicker1: TRzColorPicker;
    RzFormShape1: TRzFormShape;
    RzBorder1: TRzBorder;
    RzCalculator1: TRzCalculator;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RzBorder1Click(Sender: TObject);
    procedure RzCalendar1GetDayFormat(Sender: TObject; DayDate: TDateTime; Year,
      Month, Day: Word; var DayColor, DayFontColor: TColor;
      var DayFontStyle: TFontStyles);
    procedure FormCreate(Sender: TObject);
  private
  end;


implementation

{$R *.dfm}

uses
  RzCommon,
  SysUtils,
  DateUtils,
  MainForm;

procedure TFrmWidgets.FormClose( Sender: TObject; var Action: TCloseAction );
begin
  FrmMain.ClearWidgetRef;
  Action := caFree;
end;


procedure TFrmWidgets.FormCreate(Sender: TObject);
begin
  RzFormShape1.Picture.LoadFromFile( 'FormShapeImage.bmp' );
  RzColorPicker1.FlatColor := $005591B9;
end;


procedure TFrmWidgets.RzBorder1Click(Sender: TObject);
begin
  Close;
end;

procedure TFrmWidgets.RzCalendar1GetDayFormat(Sender: TObject;
  DayDate: TDateTime; Year, Month, Day: Word; var DayColor,
  DayFontColor: TColor; var DayFontStyle: TFontStyles);
var
  Y, M, NDOW, DOW: Word;
begin
  // If Saturday or Sunday, then highlight the day
  if DayOfWeek( DayDate ) in [ 1, 7 ] then
    DayColor := $00D2F0F5;

  if ( Month = 2 ) and ( Day = 7 ) then
  begin
    // Highlight Anniversary of Raize Software
    DayColor := $008CD2E6;
    DayFontColor := $00783C3C;
    DayFontStyle := [ fsBold ];
  end;

  DecodeDayOfWeekInMonth( DayDate, Y, M, NDOW, DOW );
  if ( DOW = 5 ) and ( NDOW = 2 ) then
  begin
    // Bold the 2nd Friday of each month
    DayFontStyle := [ fsBold ];
  end;

end;

end.



