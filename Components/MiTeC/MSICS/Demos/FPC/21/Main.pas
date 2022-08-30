unit Main;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, MSI_PerfMon;

type

  { TForm1 }

  TForm1 = class(TForm)
    lWarn: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure PerfMonThreadInterval(Sender: TPerfMonThread);
  private
    sl: TStringList;
    FPM: TPerfMonThread;
  public
  end;

var
  Form1: TForm1;

implementation

uses MiTeC_StrUtils;

{$R *.lfm}

const
  cPC1 = '\Processor(*)\% Processor Time';
  cPC2 = '\Processor Information(*)\% Processor Time';
var
  cPC: string;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
  lbl: TLabel;
  g: TProgressBar;
begin
  sl:=TStringList.Create;
  FPM:=TPerfMonThread.Create;
  cPC:=cPC1;
  FPM.GetCounterInstances(cPC,sl);
  if sl.Count=0 then begin
    cPC:=cPC2;
    FPM.GetCounterInstances(cPC,sl);
  end;
  FPM.OnInterval:=PerfMonThreadInterval;
  FPM.AddCounter(cPC);
  for i:=0 to sl.Count-1 do begin
    lbl:=TLabel.Create(Self);
    with lbl do begin
      Parent:=Self;
      if SameText(sl[i],'_Total') then
        Caption:='Total'
      else
        Caption:='Core '+sl[i];
      Top:=(Height+8)*i+10;
      Left:=10;
    end;
    g:=TProgressBar.Create(Self);
    g.Name:=Format('Gauge%d',[i]);
    g.Parent:=Self;
    g.Top:=lbl.Top;
    g.Left:=90;
    g.Height:=lbl.Height+2;
    g.Width:=Self.ClientWidth-g.Left-10;
  end;

  if sl.Count=0 then
    lWarn.Show
  else begin
    Self.ClientHeight:=lbl.Top+lbl.Height*2;
    FPM.Suspended:=False;
  end;
  Update;
end;

procedure TForm1.PerfMonThreadInterval(Sender: TPerfMonThread);
var
  i: Integer;
  g: TProgressBar;
begin
  for i:=0 to sl.Count-1 do begin
    g:=TProgressBar(FindComponent(Format('Gauge%d',[i])));
    if Assigned(g) then
      g.Position:=Round(Sender.ReadCounter(FastStringReplace(cPC,'*',sl[i])));
  end;
end;

end.
