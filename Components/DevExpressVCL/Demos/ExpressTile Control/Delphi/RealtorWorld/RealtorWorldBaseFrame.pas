unit RealtorWorldBaseFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs;

const
  IDFirst = 0;
  IDPhotos = IDFirst;
  IDResearch = 1;
  IDAgents = 2;
  IDStatistic = 3;
  IDMortgageRate = 4;
  IDLoanCalculator = 5;
  IDSystemInformation = 6;
  IDLast = IDSystemInformation;

type
  TfrmBase = class(TFrame)
  private
    { Private declarations }
  public
    procedure SelectItem(APhotoID, AAgentID: Integer); virtual;
  end;
  TfrmBaseClass = class of TfrmBase;

function GetDetailControlClass(ATag: Integer): TfrmBaseClass;
procedure RegisterFrame(AID: Integer; AFrameClass: TfrmBaseClass);

implementation

{$R *.dfm}

var
  AFrameClasses: array [IDFirst..IDLast] of TfrmBaseClass;

function GetDetailControlClass(ATag: Integer): TfrmBaseClass;
begin
  Result := AFrameClasses[ATag];
end;

procedure RegisterFrame(AID: Integer; AFrameClass: TfrmBaseClass);
begin
  AFrameClasses[AID] := AFrameClass;
end;

{ TfrmBase }

procedure TfrmBase.SelectItem(APhotoID, AAgentID: Integer);
begin
end;

end.
