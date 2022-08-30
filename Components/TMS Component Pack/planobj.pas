{$I TMSDEFS.INC}
{***********************************************************************}
{ TPlanner component                                                    }
{ for Delphi & C++Builder & Kylix                                       }
{ for Delphi & C++Builder                                               }
{                                                                       }
{ written by TMS Software                                               }
{            copyright © 1999-2012                                      }
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


unit PlanObj;

interface

uses
  Classes, Graphics;

const
  NumColors = 288;

type
  TNumColorsRange = 0..NumColors;
                                       
  TCellState = record
    Color: TColor;
    Selected: Integer;
  end;

  TPlannerColorArray = array[TNumColorsRange] of TCellState;
  PPlannerColorArray = ^TPlannerColorArray;


  TColorChangeEvent = procedure(Sender: TObject; Index: Integer) of object;

  TPlannerColorArrayList = class(TList)
  private
    FOnChange: TColorChangeEvent;
    procedure SetArray(Index: Integer; Value: PPlannerColorArray);
    function GetArray(Index: Integer): PPlannerColorArray;
  public
    constructor Create;
    destructor Destroy; override;
    property Items[Index: Integer]: PPlannerColorArray read GetArray write SetArray;
    function Add: PPlannerColorArray;
    procedure Delete(Index: Integer);
    property OnChange: TColorChangeEvent read FOnChange write FOnChange;
  end;

 


implementation

{ TPlannerColorArrayList }

function TPlannerColorArrayList.Add: PPlannerColorArray;
begin
  New(Result);
  inherited Add(Result);
end;

constructor TPlannerColorArrayList.Create;
begin
  inherited;
end;

procedure TPlannerColorArrayList.Delete(Index: Integer);
begin
  if Assigned(Items[Index]) then
    Dispose(Items[Index]);
  inherited Delete(Index);
end;

destructor TPlannerColorArrayList.Destroy;
var
  Index: Integer;
begin
  for Index := 0 to Self.Count - 1 do
    if Assigned(Items[Index]) then
      Dispose(Items[Index]);
  inherited;
end;

function TPlannerColorArrayList.GetArray(
  Index: Integer): PPlannerColorArray;
begin
  Result := PPlannerColorArray(inherited Items[Index]);
end;

procedure TPlannerColorArrayList.SetArray(Index: Integer;
  Value: PPlannerColorArray);
begin
  inherited Items[Index] := Value;
end;


end.
