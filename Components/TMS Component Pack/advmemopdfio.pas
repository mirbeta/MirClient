{***************************************************************************}
{ TAdvMemo PDF IO component                                                 }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2015                                               }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of TMS software.                                    }
{***************************************************************************}

unit AdvMemoPDFIO;

interface

{$I TMSDEFS.INC}

uses
  Classes, Windows, AdvMemo, AdvPDFLib, AdvPDFIO;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.0.1.0 : Improved : PDF rendering quality

type
  TAdvMemoPDFIO = class(TAdvPDFIOComponent)
  private
    FMemo: TAdvMemo;
    procedure CheckPageIndex(PDF: TPDFDocument);
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    function GetVersionNr: Integer; override;
    procedure GeneratePDF(AFileName: string); override;
  published
    property Memo: TAdvMemo read FMemo write FMemo;
  end;

implementation

uses
  Graphics, SysUtils, Types, JPEG, StdCtrls
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

type
  TAdvMemoEx = class(TAdvMemo);

{ TAdvGridPDFIO }

function TAdvMemoPDFIO.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvMemoPDFIO.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AComponent = FMemo) and (AOperation = opRemove) then
    FMemo := nil;
end;

var
  PI: Integer;

procedure TAdvMemoPDFIO.CheckPageIndex(PDF: TPDFDocument);
begin
  if PDF.Pages.Count > PI then
  begin
    if Assigned(OnSetHeader) then
      OnSetHeader(PDF.CurrentPage, PI);

    if Assigned(OnSetFooter) then
      OnSetFooter(PDF.CurrentPage, PI);
  end;
end;

procedure TAdvMemoPDFIO.GeneratePDF(AFileName: string);
var
  pdf: TPDFDocument;
  font: TFont;
  I,J: Integer;
  Line: TMemoLineItem;
  Part: TMemoPartItem;
  M: TPdfMarges;
  ex: TAdvMemoEx;
begin
  if not Assigned(FMemo) then
    raise Exception.Create('No Memo assigned');

  pdf := TPDFDocument.Create(Self);

  if PageLayout = plLandscape then
  begin
    pdf.Width := PageSize.Height;
    pdf.Height := PageSize.Width;
  end
  else
  begin
    pdf.Width := PageSize.Width;
    pdf.Height := PageSize.Height;
  end;

  M.Left := 15;
  M.Right := 15;
  M.Bottom := 15;
  M.Top := 15;

  pdf.CurrentPage.Marges := M;

  pdf.Header := (Header);
  if Assigned(OnSetHeader) then
    OnSetHeader(pdf.CurrentPage, pdf.Pages.Count - 1);

  pdf.Footer := (Footer);
  if Assigned(OnSetFooter) then
    OnSetFooter(pdf.CurrentPage, pdf.Pages.Count - 1);

  pdf.MetaData := MetaData;

  ex := TAdvMemoEx(FMemo);
  ex.GetLines;

  pdf.LineXPosition := M.Left;

  font := TFont.Create;
  font.Name := Memo.Font.Name;

  try
    for I := 0 to Memo.MemoLines.Count - 1 do
    begin
      Line := TMemoLineItem(Memo.MemoLines[I]);

      for J := 0 to Line.Parts.Count - 1 do
      begin
        Part := TMemoPartItem(Line.Parts[J]);
        try
          font.Color := Part.Color;
          font.Style := Part.FontStyle;
          pdf.CurrentPage.BackgroundColor := Part.BkgColor;
          PI := pdf.Pages.Count;
          pdf.CurrentPage.AddTextLine(pdf.LineXPosition, pdf.LineYPosition, Part.Text, font, alLeft, False, True, False);
          CheckPageIndex(pdf);
        finally
        end;
      end;

      pdf.CurrentPage.AddNewLine;
      CheckPageIndex(pdf);
    end;

    pdf.GeneratePDF(AFileName);
  finally
    font.Free;
    pdf.Free;
  end;
end;

end.
