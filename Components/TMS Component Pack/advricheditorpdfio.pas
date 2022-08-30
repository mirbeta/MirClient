{*************************************************************************}
{ TMS TAdvRichEditor PDF IO                                               }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2014 - 2015                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvRichEditorPDFIO;

interface

{$I TMSDEFS.INC}

uses
  Classes, Windows, AdvRichEditorBase, AdvRichEditorIO, AdvPDFLib, AdvPDFIO;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.0.1.0 : Improved : PDF rendering quality
  // v1.0.1.1 : Fixed : Background color rendering

type
  TAdvRichEditorPDFIO = class(TAdvPDFIOComponent)
  private
    FRichEditor: TAdvRichEditorBase;
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    function GetVersionNr: Integer; override;
    procedure GeneratePDF(AFileName: string); override;
  published
    property RichEditor: TAdvRichEditorBase read FRichEditor write FRichEditor;
  end;

implementation

uses
  JPEG, SysUtils, Graphics, GDIPicture, Controls;

type
  TAdvRichEditorBaseEx = class(TAdvRichEditorBase);

{ TAdvRichEditorPDFIO }

function TAdvRichEditorPDFIO.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvRichEditorPDFIO.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AComponent = FRichEditor) and (AOperation = opRemove) then
    FRichEditor := nil;
end;

procedure TAdvRichEditorPDFIO.GeneratePDF(AFileName: string);
var
  pdf: TPDFDocument;
  font: TFont;
  i: integer;
  el, el2: TREElement;
  issub,issuper,isfntclr,isbkclr: boolean;
  alignment: TAlignment;
  curindent, PI: integer;
  clr,bkclr: TColor;
  pdfalign: TPDFALIGNMENT;
  m: TPdfMarges;
  pic: TPictureElement;
  cg: TGraphicElement;
  jpg: TJpegImage;
  bmp: TBitmap;
  ms: TMemoryStream;
  url: string;
  ir: TImageRef;
  r: TRect;
  dpi: double;
  start, isInline: Boolean;
begin
  if not Assigned(RichEditor) then
    raise Exception.Create('No rich editor assigned');

  pdf := TPDFDocument.Create(Self);
  start := True;

  try
    // Set margins
    m.Top := 15;
    m.Bottom:= 15;
    m.Left:= 15;
    m.Right:= 15;
    pdf.CurrentPage.Marges := m;

    // Set dimensions
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

    // Set header & footer
    pdf.Header := (Header);
    if Assigned(OnSetHeader) then
      OnSetHeader(pdf.CurrentPage, pdf.Pages.Count - 1);

    pdf.Footer := (Footer);
    if Assigned(OnSetFooter) then
      OnSetFooter(pdf.CurrentPage, pdf.Pages.Count - 1);

    pdf.MetaData := MetaData;

    font := TFont.Create;
    font.Assign(RichEditor.Font);
    clr := RichEditor.Font.Color;
    bkclr := 0;

    issuper := false;
    issub := false;
    isfntclr := false;
    isbkclr := false;
    alignment := taLeftJustify;
    curindent := 0;
    pdfalign := TPDFALIGNMENT.alLeft;

    i := 0;
    pdf.LineXPosition := M.Left;
    url := '';

    while i < RichEditor.Context.Content.Count  do
    begin
      el := RichEditor.Context.Content.Items[i];

      if el is TLineBreakElement then
      begin
        if (i+1 < RichEditor.Context.Content.Count) then
        begin
          el2 :=  RichEditor.Context.Content.Items[i+1];
          if (el2 is TTextElement) then
          begin
            pdf.CurrentPage.FontSize := (el2 as TTextElement).FontSize;
          end;
        end;
        case pdfalign of
          TPdfAlignment.alLeft: ;
          TPdfAlignment.alRight: pdf.LineXPosition := pdf.Width - pdf.CurrentPage.TotalTextLineWidth - pdf.CurrentPage.Marges.Right;
          TPdfAlignment.alCenter: pdf.LineXPosition := Round(((pdf.Width) / 2) - (pdf.CurrentPage.TotalTextLineWidth / 2));
        end;
        PI := pdf.Pages.Count;
        pdf.CurrentPage.EndLine;
        if pdf.Pages.Count > PI then
        begin
          if Assigned(OnSetHeader) then
            OnSetHeader(pdf.CurrentPage, pdf.Pages.Count - 1);

          if Assigned(OnSetFooter) then
            OnSetFooter(pdf.CurrentPage, pdf.Pages.Count - 1);
        end;
        pdf.CurrentPage.BeginLine(el.TextColor);
      end;

      // hyperlink
      url := '';
      pdf.CurrentPage.IsUrl := False;

      if url <> el.URL then
      begin
        url := el.URL;
        if url <> '' then
        begin
          pdf.CurrentPage.IsUrl := True;

          font.Color := RichEditor.URLColor;
        end;
      end;

      pdf.CurrentPage.Url := url;

      if el.Indent <> curindent then
      begin
        curindent := el.Indent;
        if curindent > 0 then
          pdf.CurrentPage.Indenting := curindent
        else
          pdf.CurrentPage.Indenting := 0;
      end;

      if alignment <> el.Alignment then
      begin
        alignment := el.Alignment;

        case alignment of
          taLeftJustify: pdfalign := TPDFALIGNMENT.alLeft;
          taRightJustify: pdfalign := TPDFALIGNMENT.alRight;
          taCenter: pdfalign := TPDFALIGNMENT.alCenter;
        end;
      end;

      if el is TTextElement then
      begin
        if url = '' then
          font.Assign((el as TTextElement).Font);

        if not issuper and ((el as TTextElement).Baseline = TTextBaseLine.tbSuperscript)  then
        begin
          pdf.Currentpage.TextRise := 5;
          issuper := true;
        end
        else
        if issuper and not ((el as TTextElement).Baseline = TTextBaseLine.tbSuperscript) then
        begin
          pdf.Currentpage.TextRise := 0;
          issuper := false;
        end;

        if not issub and ((el as TTextElement).Baseline = TTextBaseLine.tbSubscript) then
        begin
          pdf.Currentpage.TextRise := -5;
          issub := true;
        end
        else
        if issub and not ((el as TTextElement).Baseline = TTextBaseLine.tbSubscript) then
        begin
          pdf.Currentpage.TextRise := 0;
          issub := false;
        end;

        if (clr <> (el as TTextElement).TextColor) then
        begin
          if isfntclr then
          begin
            font.Color := RichEditor.Font.Color;
            isfntclr := false;
          end;

          clr := (el as TTextElement).TextColor;

          if clr <> RichEditor.Font.Color then
          begin
            font.Color := clr;
            isfntclr := true;
          end;
        end;

        if (bkclr <> (el as TTextElement).Color) then
        begin
          if isbkclr then
          begin
            pdf.Currentpage.BackgroundColor := clNone;
            isbkclr := false;
          end;

          bkclr := (el as TTextElement).Color;

          if bkclr <> clr then
          begin
            pdf.Currentpage.BackgroundColor := bkclr;
            isbkclr := true;
          end;
        end;

        if el.Text <> '' then
        begin
          if start then
          begin
            pdf.CurrentPage.BeginLine(el.TextColor);
            start := False;
          end;
          pdf.CurrentPage.AddText(el.Text, font, pdfalign);
        end;
      end;

      if (el is TPictureElement) then
      begin
        pic := (el as TPictureElement);

        bmp := TBitmap.Create;
        jpg := TJPEGImage.Create;

        isInline := False;
        if (I-1 >= 0) then
        begin
          isInline := Not ( RichEditor.Context.Content.Items[I-1] is TLineBreakElement);

        end;

        try
          bmp.Width := pic.PictureWidth;
          bmp.Height := pic.PictureHeight;

          bmp.Canvas.StretchDraw(Bounds(0, 0, bmp.Width, bmp.Height), pic.Picture);

          jpg.CompressionQuality := pdf.ImageQuality;
          jpg.Performance := jpBestQuality;
          jpg.PixelFormat := jf24Bit;
          jpg.Assign(bmp);

          ms := TMemoryStream.Create;

          jpg.SaveToStream(ms);
          ms.Position := 0;

          if (isInline) then
          begin
            pdf.CurrentPage.EndLine(False);
            pdf.CurrentPage.AddJpeg(pdf.CurrentPage.TotalTextLineWidth + pdf.LineXPosition + bmp.Width + M.Left, pdf.LineYPosition - FONTSIZE, bmp.Width, bmp.Height, ms, '1', pdfalign, False, True);
            pdf.LineXPosition := pdf.LineXPosition + pdf.CurrentPage.TotalTextLineWidth + bmp.Width + 5;
            pdf.CurrentPage.BeginLine(el.TextColor);
          end
          else
          begin
            pdf.CurrentPage.AddJpeg(pdf.LineXPosition, pdf.LineYPosition, bmp.Width, bmp.Height, ms, '1', pdfalign);
          end;

          ir.ImageFileName := AFileName;
          ir.Image := ms;
          pdf.CurrentPage.ImagesList.Add(ir);

        finally
          jpg.Free;
          bmp.Free;
        end;
      end
      else

      if (el is TGraphicElement) then
      begin
        cg := (el as TGraphicElement);

        dpi := cg.DPIratio;

        r := Rect(0,0,Round(cg.Size.cx * dpi) * 2, Round(cg.Size.cy * dpi) * 2);

        InflateRect(r, -Round(PICTURE_MARGIN * dpi) div 2, -Round(PICTURE_MARGIN * dpi) div 2);

        bmp := TBitmap.Create;
        jpg := TJPEGImage.Create;

        try
          bmp.Width := r.Right;
          bmp.Height := r.Bottom;

          TAdvRichEditorBaseEx(RichEditor).DrawGraphic(bmp.Canvas, r, cg.ID);

          jpg.CompressionQuality := pdf.ImageQuality;
          jpg.Assign(bmp);

          ms := TMemoryStream.Create;

          jpg.SaveToStream(ms);
          ms.Position := 0;

          pdf.CurrentPage.AddJpeg(pdf.LineXPosition, pdf.LineYPosition, bmp.Width div 2, bmp.Height div 2, ms, '1', pdfalign);

          ir.ImageFileName := AFileName;
          ir.Image := ms;
          pdf.CurrentPage.ImagesList.Add(ir);

        finally
          jpg.Free;
          bmp.Free;
        end;
      end;


      if el is TBulletElement then
      begin
        if (el as TBulletElement).&Type = btCircle then
          pdf.Currentpage.BulletStyle := bsCircle
        else if (el as TBulletElement).&Type = btSquare then
          pdf.Currentpage.BulletStyle := bsSquare
        else if (el as TBulletElement).&Type = btArrow then
          pdf.Currentpage.BulletStyle := bsArrow
        else if (el as TBulletElement).&Type = btStar then
          pdf.Currentpage.BulletStyle := bsStar
        else if (el as TBulletElement).&Type = btTick then
          pdf.Currentpage.BulletStyle := bsCheck;
      end;

      inc(i);
    end;

    pdf.CurrentPage.EndLine(False);

    pdf.GeneratePDF(AFileName);
    font.Free;

  finally
    pdf.Free;
  end;
end;

end.
