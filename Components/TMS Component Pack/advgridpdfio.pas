{***************************************************************************}
{ TAdvStringGrid PDF IO component                                           }
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

unit AdvGridPDFIO;

{$I TMSDEFS.INC}

interface

uses
  Classes, AdvGrid, AdvPDFLib, AdvPDFIO;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 3; // Release nr.
  BLD_VER = 5; // Build nr.

  // Version history
  // v1.0.0.0 : First release
  // v1.1.0.0 : New : FitToPage added
  //          : New : Scale added
  // v1.1.0.1 : Fixed : Center align fix
  //          : Fixed : Image Scaling
  // v1.1.0.2 : Improved : GridLineColor used now for PDF export
  // v1.1.0.3 : Fixed : Issue with PDF export for grid with hidden columns
  // v1.1.1.0 : New : Save; overload function
  //          : New : AutoOpen property added
  //          : New : SaveDialogTitle property added
  // v1.1.1.1 : Fixed : Table overflow
  //          : Fixed : Table over footer
  //          : Fixed : More precize wordwrap
  //          : Fixed : Gradients
  //          : Fixed : rounded numbers
  //          : Fixed : too long text will not overlap the cell beneath
  //          : Fixed : Fixed cell Gradients
  // v1.1.1.2 : Improved : HTML image align
  //          : Improved : Cell center align
  //          : Improved : Cell text wrap
  // v1.1.2.0 : Improved : New and improved way to export a fixed grid
  //          : Improved : Text-to-cell more crisp
  //          : Improved : Grid-to-PDF more fitting
  //          : Improved : cells contain less 'air'
  //          : Improved : Text wrapping
  //          : Improved : Header & Footer Alignments
  //          : New      : AddText function
  // v1.1.3.0 : Improved : Overall performance, especially with large grids
  //          : New : New property page size
  //          : New : OnGetRow event for manual pagebreaks
  //          : New : Header and Footer events
  // v1.1.3.1 : Fixed : Issue with DecimalSeparator & ThousandSeparator handling
  // v1.1.3.2 : Fixed : Regression with DecimalSeparator handling
  // v1.1.3.3 : Fixed : HTML incorrect linebreaks
  // v1.1.3.4 : Fixed : Images in merged cells
  // v1.1.3.4 : Improved: Cell alignment in merged cells
  // v1.1.3.5 : Fixed : Image Resizing

type
  TOnGetRowEvent = procedure(Sender: TObject; ARow: Integer; var IsPageBreak: Boolean) of object;

  TAdvGridPDFIO = class(TAdvPDFIOComponent)
  private
    FGrid: TAdvStringGrid;
    FScale: Single;
    FFitToPage: Boolean;
    FOnGetRow: TOnGetRowEvent;
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    function GetVersionNr: Integer; override;
    procedure DrawFixedTable(Table: TPDFTableItem);
    procedure GeneratePDF(AFileName: string); override;
    procedure DoOnGetRow(Arow: integer; var IsPageBreak: boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property FitToPage: Boolean read FFitToPage write FFitToPage default false;
    property Grid: TAdvStringGrid read FGrid write FGrid;
    property Scale: Single read FScale write FScale;
    property OnGetRow: TOnGetRowEvent read FOnGetRow write FOnGetRow;
  end;


implementation

uses
  Windows, Graphics, SysUtils, BaseGrid, Grids, Advobj, Types, JPEG, AdvUtil, StdCtrls, Generics.Collections, Math
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;


{ TAdvGridPDFIO }

constructor TAdvGridPDFIO.Create(AOwner: TComponent);
begin
  inherited;
  FScale := 1;
  FFitToPage := False;
end;

function TAdvGridPDFIO.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvGridPDFIO.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AComponent = FGrid) and (AOperation = opRemove) then
    FGrid := nil;
end;

procedure TAdvGridPDFIO.GeneratePDF(AFileName: string);
var
  c,r,w,h: integer;
  pdf: TPDFDocument;
  table: TPDFTableItem;
  tri: TPDFTableRowItem;
  tci: TPDFTableColumnItem;
  trci: TPDFTableRowCellItem;
  cp: TCellProperties;
  p: TPoint;
  pw, ph: Integer;

  AState: TGridDrawState;
  AColorTo,AMirrorColor,AMirrorColorTo: TColor;
  HAlignment: TAlignment;
  VAlign: TVAlignment;
  GD: TCellGradientDirection;
  TWW: Boolean;
  ABrush: TBrush;
  AFont: TFont;
  Pic: TCellGraphic;
  membmp: Graphics.TBitmap;
  mempic: TPicture;
  memico: TIcon;
  Cr: TCellGraphic;
  tt: TTextType;
  state: TCheckBoxState;
  m: TPdfMarges;
  SCALING: Single;
  isPageBreak: boolean;
  ds,ts: char;

begin
  if not Assigned(FGrid) then
    raise Exception.Create('No grid assigned');

  pdf := TPDFDocument.Create(Self);

  ds := FormatSettings.DecimalSeparator;
  ts := FormatSettings.ThousandSeparator;

  try
    if PageLayout = plLandscape then
    begin
      pdf.Width := PageSize.Height;
      pdf.Height := PageSize.Width;
    end
    else
    begin
      pdf.Width := PageSize.Width;// 612;
      pdf.Height := PageSize.Height; //775;
    end;

    pdf.ImagesList := Grid.GridImages;

    // Set Marges
    m.Top := 15;
    m.Bottom:= 15;
    m.Left:= 15;
    m.Right:= 15;

    pdf.CurrentPage.Marges := m;

    pdf.Header := (Header);
    if Assigned(OnSetHeader) then
      OnSetHeader(pdf.CurrentPage, pdf.Pages.Count - 1);

    pdf.Footer := (Footer);
    if Assigned(OnSetFooter) then
      OnSetFooter(pdf.CurrentPage, pdf.Pages.Count - 1);

    pdf.MetaData := MetaData;

    w := 0;
    if FitToPage then
    begin
      for c := 0 to FGrid.ColCount - 1 do
      begin
        w := w + FGrid.ColWidths[c];
      end;

      SCALING :=  (pdf.Width - m.Left - m.Right) / (w +  m.Left + m.Right);

      Scale := SCALING;
    end;

    table := pdf.CurrentPage.Tables.Add;
    table.X := 10;
    table.Y := pdf.Height - pdf.LineYPosition;
    table.Width := Grid.Width;
    if FitToPage then
      table.Width := pdf.Width - m.Left - m.Right;

    for r := 0 to FGrid.RowCount - 1 do
    begin
      tri := table.Rows.Add;
      tri.Height := Round((FGrid.RowHeights[r] * Scale));

      IsPageBreak := false;
      DoOnGetRow(r, IsPageBreak);
      tri.PageBreakAfter := IsPageBreak;

      for c := 0 to FGrid.ColCount - 1 do
      begin

        if r = 0 then
        begin
          tci := table.Columns.Add;
          tci.Width := Round((Grid.ColWidths[c] * Scale));
        end;

        trci := tri.Cells.Add;
        cp := Grid.GetCellProperties(c,r);

        trci.BorderColor := cp.BorderColor;
        if (trci.BorderColor = clBlack) and (trci.BorderColor <> Grid.GridLineColor) then
          trci.BorderColor := Grid.GridLineColor;
        trci.UrlColor := Grid.URLColor;

        P := Grid.CellSpan(c,r);
        trci.IsMerged := P.X + P.Y > 0;
        if (P.X = c) and (P.Y = r) and (P.X + P.Y > 0) then
          trci.IsMerged := True;
        if trci.IsMerged then
        begin
          trci.BaseCell := grid.BaseCell(c,r);
          trci.CellSpan := grid.CellSpan(c,r);
        end;
        TWW := True;

        {$REGION 'Background Color & alignment'}
        ABrush := TBrush.Create;
        AFont := TFont.Create;
        try
          Grid.GetVisualProperties(c, r, AState, False, False, True, ABrush, AColorTo, AMirrorColor, AMirrorColorTo, AFont, HAlignment, VAlign, TWW, GD);
          trci.HAlign := HAlignment;
          trci.VAlign := TPdfVerticalAlignment(VAlign);
          trci.BackgroundColor := ABrush.Color;
          trci.BackgroundColorFrom := ABrush.Color;
          trci.BackgroundColorTo := AColorTo;
          trci.BackgroundMirrorColorTo := AMirrorColorTo;
          trci.BackgroundMirrorColorFrom := AMirrorColor;
          trci.BackgroundOrientation := gd = GradientVertical;
          if AMirrorColorTo <> clNone then
            trci.BackgroundOrientation := True;

          AFont.Size := Round(AFont.Size * Scale);

          trci.TextFont.Assign(AFont);
        finally
          ABrush.Free;
          AFont.Free;
        end;
        {$ENDREGION}

        {$REGION 'Text Types'}
        trci.Text := Grid.DisplCells[Grid.RealColIndex(c),r];
        tt := TextType(trci.Text);
        case tt of
          ttText:
          begin
            //
          end;
          ttHTML:
          begin
            trci.HTMLText := trci.Text;
            trci.HAlign := taLeftJustify;
            trci.VAlign := TPdfVerticalAlignment(vtaTop);
          end;
          ttURL:
          begin
            trci.Url := trci.Text;
          end;
        end;
        {$ENDREGION}

        {$REGION 'IMAGES'}
        Pic := Grid.CellGraphics[c, r];
        if (Pic <> nil) and (Pic.CellBitmap <> nil) then
        begin
          if (Pic.CellType = ctBitmap) then
          begin
            Cr := Grid.CellGraphics[c, r];
            trci.ImageType := citBitmap;
            membmp := Graphics.TBitmap.Create;
            membmp.Assign(pic.CellBitmap);
            pw := Round(membmp.Width * Scale);
            ph := Round(membmp.Height * Scale);
            membmp.Canvas.FillRect(Rect(0, 0, pw, ph));
            membmp.Width := pw;
            membmp.Height := ph;
            membmp.Canvas.StretchDraw(Bounds(0, 0, pw, ph), pic.CellBitmap);

            trci.Bitmaps.Add(membmp);
            trci.ImageHAlign := TPdfCellHAlign(Cr.CellHAlign);
            trci.ImageVAlign := TPdfCellVAlign(Cr.CellVAlign);
          end;

          if (Pic.CellType in [ctPicture]) and (TPicture(Pic.CellBitmap).Graphic <> nil) then
          begin
            Cr := Grid.CellGraphics[c, r];
            trci.ImageType := citPicture;
            membmp := Graphics.TBitmap.Create;
            membmp.Assign(TPicture(Pic.CellBitmap).Graphic);
            pw := Round(membmp.Width * Scale);
            ph := Round(membmp.Height * Scale);
            membmp.Canvas.FillRect(Rect(0, 0, pw, ph));
            membmp.Width := pw;
            membmp.Height := ph;
            membmp.Canvas.StretchDraw(Bounds(0, 0, pw, ph), TPicture(Pic.CellBitmap).Graphic);
            trci.Bitmaps.Add(membmp);
            trci.ImageHAlign := TPdfCellHAlign(Cr.CellHAlign);
            trci.ImageVAlign := TPdfCellVAlign(Cr.CellVAlign);
          end;

          if (Pic.CellType in [ctFilePicture]) then
          begin
            Cr := Grid.CellGraphics[c, r];
            trci.ImageType := citFilePicture;
            mempic := TPicture.Create;
            mempic.LoadFromFile(TFilePicture(Pic.CellBitmap).Filename);
            membmp := Graphics.TBitmap.Create;
            membmp.Assign(mempic.Bitmap);
            pw := Round(membmp.Width * Scale);
            ph := Round(membmp.Height * Scale);

            membmp.Canvas.FillRect(Rect(0, 0, pw, ph));
            membmp.Width := pw;
            membmp.Height := ph;
            membmp.Canvas.StretchDraw(Bounds(0, 0, pw, ph), mempic.Bitmap);
            trci.Bitmaps.Add(membmp);
            trci.ImageType := citFilePicture;
            trci.ImageHAlign := TPdfCellHAlign(Cr.CellHAlign);
            trci.ImageVAlign := TPdfCellVAlign(Cr.CellVAlign);

            mempic.Free;
          end;
        end;

        if (Pic <> nil) and (Pic.CellType = ctImageList) then
          begin
             memico := TIcon.Create;
             try
              Grid.GridImages.GetIcon(Pic.CellIndex, memico);
              if Assigned(memico) and not memico.Empty then
              begin
                membmp := Graphics.TBitmap.Create;
                membmp.Width := Round(memico.Width * Scale);
                membmp.Height := Round(memico.Height * Scale);
                membmp.Canvas.Draw(0,0, memico);
                membmp.Transparent := true;
                Cr := Grid.CellGraphics[c, r];
                trci.ImageType := citImageList;
                trci.Bitmaps.Add(membmp);

                trci.ImageHAlign := TPdfCellHAlign(Cr.CellHAlign);
                trci.ImageVAlign := TPdfCellVAlign(Cr.CellVAlign);
              end;
            finally
            begin
              memico.Free;
            end;
          end;
        end;

        if (Pic <> nil) and (Pic.CellType = ctImages) and
          (TIntList(Pic.CellBitmap).Count > 0) then
        begin
          memico := TIcon.Create;
          try
            Cr := Grid.CellGraphics[c, r];
            trci.ImageType := citImages;
            Grid.GridImages.GetIcon(TIntList(Pic.CellBitmap).Items[0], memico);
            if Assigned(memico) and not memico.Empty then
            begin
              membmp := Graphics.TBitmap.Create;
              membmp.Canvas.Draw(0,0, memico);
              membmp.Transparent := true;
              pw := Round(memico.Width * Scale);
              ph := Round(memico.Height * Scale);

              membmp.Canvas.FillRect(Rect(0, 0, pw, ph));
              membmp.Width := pw;
              membmp.Height := ph;
              membmp.Canvas.StretchDraw(Bounds(0, 0, pw, ph), memico);
              trci.Bitmaps.Add(membmp);
              trci.ImageType := citImages;
              trci.ImageHAlign := TPdfCellHAlign(Cr.CellHAlign);
              trci.ImageVAlign := TPdfCellVAlign(Cr.CellVAlign);
            end;
          {$REGION 'TODO'}
            // for I := 0 to TIntList(Pic.CellBitmap).Count - 1 do
            // begin
            // if Grid.GridImages.GetBitmap(TIntList(Pic.CellBitmap).Items[I], membmp) then
            // begin
            // trci.Bitmaps.Add(membmp);
            // trci.ImageHAlign := Cr.CellHAlign;
            // trci.ImageVAlign := Cr.CellVAlign;
            // end;
            // end;
          {$ENDREGION}
          finally
            begin
               memico.Free;
            end;
          end;
        end;
        {$ENDREGION}

        if grid.GetCheckboxState(c,r,state) then
        begin
          trci.IsCheckbox := True;
          trci.Checked := state = cbChecked;
        end;

      end;
    end;
    table.TableType := ttWysiwyg;

    // For now, we just support wysiwyg method
    DrawFixedTable(table);


    pdf.GeneratePDF(AFileName);
  finally
    pdf.Free;
    FormatSettings.DecimalSeparator := ds;
    FormatSettings.ThousandSeparator := ts;
  end;
end;

procedure TAdvGridPDFIO.DoOnGetRow(Arow: integer; var IsPageBreak: boolean);
begin
  if Assigned(OnGetRow) then
    OnGetRow(Self, Arow, IsPageBreak);
end;

procedure TAdvGridPDFIO.DrawFixedTable(Table: TPDFTableItem);
var
  Cell: TPDFTableRowCellItem;
  C, BG, Tmp, LTemp, Cb, Shading: string;
  CL: LongInt;
  samePage: Boolean;
  R,RR,G,GG,B,BB,BGR,BGG,BGB,SBGG1, SBGR1, SBGB1, SBGG2, SBGR2, SBGB2: string;
  shadeText: string;
  I, II, TII, J, JJ, TJJ, Imgs, PosX, PosY, LY, D, H, DW, DH, OCC, LM: Integer;
  alX,alY,textw, mcw, mch, temp: Integer;
  imgX, imgY: Integer;
  tempfont: TFont;
  pg: TPDFPageItem;
  Rws,Cls: TList<Integer>;
  RCount,CCount: Integer;
  Tw,TH: Integer;
  HReps,VReps: Integer;
  M,Mtemp: TPdfMarges;
  P: TPDFPageItem;
  Pt: TPoint;
  counter: Integer;
  jpg: TJPegImage;
  ms: TMemoryStream;
  ir: TImageRef;
  bmp: TBitmap;
  Shade: TGradientPatern;
  cc,rc,tot: Integer;
  textAdvmnt: Integer;
  html, html2: THTMLRef;
  mdh, tc: Integer;
  rectW, rectH: Integer;
  mpo: TMultiPageOption;
  count: Integer;
  isBaseCell, IsNewLine: Boolean;
begin
  P := table.PDFPage.PDFDocument.CurrentPage;

  C := P.Content;

  /// Text Color
  table.PDFPage.PDFDocument.GetRGBColor(Table.Color, R, G, B);

  PosX := Table.X;
  PosY := P.PDFDocument.Height - Table.Y;

  DW := P.PDFDocument.Width;
  DH := P.PDFDocument.Height;
  LM := P.Marges.Left;

  Rws := TList<Integer>.Create;
  Cls := TList<Integer>.Create;

  shadeText := '';

  alX := 0;
  alY := 0;
  textw := 0;
  textAdvmnt := 0;
  Mtemp := P.Marges;
  MTemp.Top := 0;

  {$REGION 'TABLE CALCULATIONS'}
  /// Calculate full table width
  Tw := 0;
  count := 0;
  for I := 0 to Table.Columns.Count - 1 do
  begin
    if count + Table.Columns[I].Width  > DW - P.Marges.Left - P.Marges.Right then
    begin
      Inc(HReps);
      Cls.Add(I);
      count := 0;
    end;

    count := count + Table.Columns[I].Width;
    Tw := Tw + Table.Columns[I].Width;
  end;
  if HReps = 0 then
  begin
    HReps := 1;
    Cls.Add(Table.Columns.Count);
  end;
  if Cls.Last <> Table.Columns.Count then
  begin
    Cls.Add(Table.Columns.Count );
    Inc(Hreps);
  end;

  /// Calculate full table height
  Th := 0;
  count := 0;
  for I := 0 to Table.Rows.Count - 1 do
  begin
    if (Count + Table.Rows[I].Height > DH - P.Marges.Top - P.Marges.Bottom) or (Table.Rows[I].PageBreakAfter) then
    begin
      Inc(VReps);
      Rws.Add(I);
      count := 0;
    end;

    count := count + Table.Rows[I].Height;
    Th := Th + Table.Rows[I].Height;
  end;

  if VReps = 0 then
  begin
    VReps := 1;
    Rws.Add(Table.Rows.Count);
  end;

  if Rws.Last <> Table.Rows.Count then
  begin
    Rws.Add(Table.Rows.Count );
    Inc(Vreps);
  end;
  {$ENDREGION}

  // loop loop loop loop
  for J := 0 to VReps - 1 do
  begin
    for I := 0 to HReps - 1 do
    begin
      if (J = 0) and (I = 0) then
      begin
        TII := 0;
        TJJ := 0;
      end
      else
      begin
        if I <> 0 then TII := Cls[I-1] else TII := 0;;
        if J <> 0 then TJJ := Rws[J-1] else TJJ := 0;
      end;

      for JJ := TJJ to Rws[J] - 1 do
      begin
        if JJ > TJJ then
        begin
          PosY := PosY + Table.Rows[JJ-1].Height;
          PosX := Table.X;
        end;

      {$REGION 'Rows'}
        for II := TII to Cls[I] - 1 do
        begin
        {$REGION 'Cols'}
          Cell := Table.Rows[JJ].Cells[II];
          Cell.Page := p.ID;
          samePage := Cell.Page = Table.Rows[Cell.BaseCell.Y].Cells[Cell.BaseCell.X].Page;
          if Cell.TextFont = nil then
            Cell.TextFont.Assign(p.PDFDocument.DefaultFont);

          {$REGION 'HTML'}
          if Cell.HTMLText <> '' then
          begin
            Cell.Text := P.HtmlToPdf(Cell.HTMLText, Cell);
          end;
          {$ENDREGION}

          {$region 'Painting & Colors'}
          P.PDFDocument.GetRGBColor(Cell.BorderColor, R, G, B);

          if II > TII then
            PosX := PosX + Table.Columns[II-1].Width;

          // draw the cells
          rectW := Table.Columns[II].Width;
          rectH := PosY + Table.Rows[JJ].Height;

          {$region 'Merging'}
          isBaseCell := (Cell.IsMerged) and ( (Cell.BaseCell.X = II) and (Cell.BaseCell.Y = JJ) );

          mpo := mpoNone;
          if isBaseCell then
          begin
            for count := 1 to Cell.CellSpan.X do
            begin
              if II+count <= Cls[I] - 1 then
              begin
                mpo := mpoNone;
                rectW := rectW + Table.Columns[II+count].Width;
              end
              else
              begin
                mpo := mpoHorizontal;
              end;
            end;

            for count := 1 to Cell.CellSpan.Y do
            begin
              if JJ+count <= Rws[J] - 1 then
              begin
                mpo := mpoNone;
                rectH := rectH + Table.Rows[JJ+count].Height;
              end
              else
              begin
                mpo := mpoVertical;
              end;
            end;
          end;
          {$ENDREGION}

          if (not Cell.IsMerged) or (isBaseCell) or ((isBaseCell) and (Cell.Text <> '')) then
          begin
            shadeText := P.DrawRectangle(PosX, PosY, rectH, rectW, Cell.BorderColor, Cell.BackgroundColor, Cell.BackgroundColorTo, Cell.BackgroundMirrorColorFrom, Cell.BackgroundMirrorColorTo, Cell.BackgroundOrientation, P.ID, mpo, False);
          end;

          if Table.Rows[Cell.BaseCell.Y].Cells[Cell.BaseCell.X] = Cell then
            Cell.GradientRef := shadeText
          else
          begin
            if samePage then
            begin
              if (Cell.BaseCell.Y = 0) and (Cell.BaseCell.X = 0) then
                Cell.GradientRef := shadeText
              else
                Cell.GradientRef := Table.Rows[Cell.BaseCell.Y].Cells[Cell.BaseCell.X].GradientRef;
            end
            else
            begin
              cell.GradientRef := shadeText;
              Table.Rows[Cell.BaseCell.Y].Cells[Cell.BaseCell.X].GradientRef := Cell.GradientRef;
            end;
          end;

          {$ENDREGION}

          {$REGION 'IMAGES'}
          for Imgs := Cell.Bitmaps.Count - 1 downto 0 do
          begin
             mcw := Table.Columns[II].Width + 10;

            /// Calculate the merged cells width
            for counter := Cell.BaseCell.X to Cell.BaseCell.X + Cell.CellSpan.X do
            begin
              mcw := mcw + Table.Columns[counter].Width;
            end;

            mch := Table.Rows[JJ].Height;

            for counter := Cell.BaseCell.Y to Cell.BaseCell.Y + Cell.CellSpan.Y-1 do
            begin
              mch := mch + Table.Rows[counter].Height;
            end;

            jpg := TJPEGImage.Create;
            bmp := TBitmap.Create;
            bmp.Width := Ceil(Cell.Bitmaps.Items[Imgs].Width);
            if bmp.Width > mcw then
              bmp.Width := mcw;
            bmp.Height := Ceil(Cell.Bitmaps.Items[Imgs].height);
            if bmp.Height > mch then
              bmp.Height := mch;
            bmp.Canvas.Brush.Color := Cell.BackgroundColor;
            bmp.Canvas.Brush.Style := bsSolid;
            bmp.Canvas.FillRect(Rect(0,0, bmp.Width, bmp.Height));
            bmp.Canvas.StretchDraw(Bounds(0, 0, bmp.Width, bmp.Height), Cell.Bitmaps.Items[Imgs]);
            jpg.Assign(bmp);
            textAdvmnt := 0;

            ms := TMemoryStream.Create;
            jpg.SaveToStream(ms);
            ms.Position := 0;
            bmp.Free;

            imgX := alX + 3;
            imgY := PosY;
            {$REGION 'Horizontal Align'}
            case Cell.ImageHAlign of
              haLeft:
              begin
                imgX := posX + 3;
              end;
              haRight:
              begin
                p.PDFDocument.Width := p.PDFDocument.Width + 20;
                imgX := posX + Table.Columns[II].Width - 20;
              end;
              haCenter:
              begin
                imgX := posX + Round(Table.Columns[II].Width/2) - 10;
              end;
              haBeforeText:
              begin
                imgX := posX + 2;
                textAdvmnt := bmp.Width;
              end;
              haAfterText:
              begin
                imgX := posX + textw + 2;
                textAdvmnt := bmp.Width + 2;
              end;
              haFull: imgX := alX;
            end;
            imgX := imgX + 3;
            {$ENDREGION}

            {$REGION 'Vertical Alignment'}
            case Cell.ImageVAlign of
              vaTop:
              begin
                imgY := posY + 3;
              end;
              vaBottom:
              begin
                imgY := imgY + Table.Rows[JJ].Height - bmp.Height - 3;
              end;
              vaCenter:
              begin
                  imgY := PosY +  Round(Table.Rows[JJ].Height / 2) - Round(bmp.Height / 2);
              end;
              vaUnderText:
              begin
                imgY := alY + Cell.TextFont.Size - 3;
              end;
              vaAboveText:
              begin
                alY := alY + bmp.Height + Cell.TextFont.Size + 3;
//                imgY := imgY - 2*Cell.TextFont.Size;
                imgY := imgY + 3;
              end;
              vaFull:;
            end;
            {$ENDREGION}

            for cc := 0 to Cell.HTML.Count - 1 do
            begin
              if (Cell.HTML[cc].HasImage) and (Cell.HTML[cc].ImgIdx = imgs) then
              begin
                imgY := imgY + 5;
                imgX := PosX + 5;
                if cc <> 0 then
                  imgX :=  cell.HTML[cc-1].X + Round(p.GetWidthText(cell.HTML[cc-1].Text, cell.TextFont)) + 3;
              end;
            end;

            P.AddJpeg(imgX, imgY, bmp.Width, bmp.Height, ms, '1', alLeft);

            jpg.Free;

            ir.Image := ms;
            P.ImagesList.Add(ir);
            if Cell.Bitmaps.Count > 1 then
              alX := alX - bmp.Width;
          end;
          {$ENDREGION}

          {$region 'text'}
            mcw := 0;
            mch := 0;
            alX := PosX;
            alY := posY + 2;
            textw := P.GetWidthText(Cell.Text, Cell.TextFont);

            {$REGION 'ALIGNMENTS'}
            /// Include text alignment
            case Cell.HAlign of
              taLeftJustify:
              begin
              {$REGION 'Merged'}
                if Cell.IsMerged then
                begin
                  if (Cell.BaseCell.X = II) and (Cell.BaseCell.Y = JJ) then
                  begin
                    mcw := 0;

                    /// Calculate the merged cells width
                    for counter := Cell.BaseCell.X to Cell.BaseCell.X + Cell.CellSpan.X do
                    begin
                      mcw := mcw + Table.Columns[counter].Width;
                      P.PDFDocument.TextWidth := P.PDFDocument.TextWidth + Round(Table.Columns[counter].Width);
                    end;

                    P.PDFDocument.TextWidth := P.PDFDocument.TextWidth + Round(Table.Columns[II].Width);

                    alx := posX + 3;

                    mch := 0;

                    for counter := Cell.BaseCell.Y to Cell.BaseCell.Y + Cell.CellSpan.Y-1 do
                    begin
                      mch := mch + Table.Rows[counter].Height;
                    end;

                    case Cell.VAlign of
                      TPdfVerticalAlignment.vtaCenter:
                      begin

                        alY := posY + Round(mch/2);
                      end;
                      TPdfVerticalAlignment.vtaBottom:
                      begin
                        alY := posY + Round(mch) + 2;//- ( cell.TextFont.Size));
                      end;
                    end;
                  end
                  else
                  begin
                    Cell.Text := '';
                  end;
                end
                {$ENDREGION}
                else
                  alX := alX + 3;
              end;
              taCenter:
              begin
                {$REGION 'Merged'}
                if Cell.IsMerged then
                begin
                  if (Cell.BaseCell.X = II) and (Cell.BaseCell.Y = JJ) then
                  begin
                    mcw := 0;

                    /// Calculate the merged cells width
                    for counter := Cell.BaseCell.X to Cell.BaseCell.X + Cell.CellSpan.X do
                    begin
                      mcw := mcw + Table.Columns[counter].Width;
                      P.PDFDocument.TextWidth := P.PDFDocument.TextWidth + Round(Table.Columns[counter].Width);
                    end;

                    P.PDFDocument.TextWidth := P.PDFDocument.TextWidth + Round(Table.Columns[II].Width);    // here

                    /// Place the first cells text in the horizontal middle
                    alx := posX + Round(mcw/2);// - Round(textw/2);

                    case Cell.VAlign of
                      TPdfVerticalAlignment.vtaCenter:
                      begin
                        mch := 0;

                        for counter := Cell.BaseCell.Y to Cell.BaseCell.Y + Cell.CellSpan.Y-1 do
                        begin
                          mch := mch + Table.Rows[counter].Height;
                        end;

                        alY := posY + Round(mch/2);
                      end;
                      TPdfVerticalAlignment.vtaBottom:
                      begin

                      end;
                    end;
                  end
                  else
                  begin
                    Cell.Text := '';
                  end;
                end
                {$ENDREGION}
                else
                begin
                  alX := (alX + Table.Columns[II].Width) - Round(Table.Columns[II].Width/2) - Round(textw/2);
                end;
              end;
              taRightJustify:
              begin
                {$REGION 'Merged'}
                if Cell.IsMerged then
                begin
                  if (Cell.BaseCell.X = II) and (Cell.BaseCell.Y = JJ) then
                  begin
                    mcw := 0;

                    /// Calculate the merged cells width
                    for counter := Cell.BaseCell.X to Cell.BaseCell.X + Cell.CellSpan.X do
                    begin
                      mcw := mcw + Table.Columns[counter].Width;
                      P.PDFDocument.TextWidth := P.PDFDocument.TextWidth + Round(Table.Columns[counter].Width);
                    end;

                    /// Place the first cells text in the horizontal middle
                    alx := posX + mcw - 5;// Round(textw) - 5;

                    case Cell.VAlign of
                      TPdfVerticalAlignment.vtaCenter:
                      begin
                        mch := 0;

                        for counter := Cell.BaseCell.Y to Cell.BaseCell.Y + Cell.CellSpan.Y - 1 do
                        begin
                          mch := mch + Table.Rows[counter].Height;
                        end;

                        alY := posY + Round(mch/2);
                      end;
                      TPdfVerticalAlignment.vtaBottom:
                      begin

                      end;
                    end;
                  end
                  else
                  begin
                    Cell.Text := '';
                  end;
                end
                {$ENDREGION}
                else
                begin
                  alX := posX + Table.Columns[II].Width - textw - 5;
                end;
              end;
            end;

            case Cell.VAlign of
              TPdfVerticalAlignment.vtaTop:
              begin
                alY := alY + Cell.TextFont.Size;
              end;
              TPdfVerticalAlignment.vtaCenter:
              begin
                alY := alY + Table.Rows[JJ].Height - Round(Table.Rows[JJ].Height / 2) + 2;
              end;
              TPdfVerticalAlignment.vtaBottom:
              begin
                alY := alY + Table.Rows[JJ].Height - 5;
//                if textw > Table.Columns[II].Width then
//                  alY := alY -  Round( ((textw / Table.Columns[II].Width) * Cell.TextFont.Size) / 2) + 5 ;
              end;
            end;

            if alY < posY then
              alY := posY + 2 + Cell.TextFont.Size;

            {$ENDREGION}

            {$REGION 'URLS'}
              if Cell.Url <> '' then
              begin
                P.Url := Cell.Url;
                CL := Cell.TextFont.Color;
                Cell.TextFont.Color := Cell.UrlColor;
              end;
            {$ENDREGION}

            /// ------------------- Add the text -------------------
          IsNewLine := False;
          if Cell.HTMLText <> '' then
          begin
          {$REGION 'HTML'}
            /// If it's some html text
            Tmp := '';
            Cell.Text := '';
            cc := 0;

            for html in Cell.HTML do
            begin
              tempfont := TFont.Create;
              tempfont.Name := Cell.TextFont.Name;
              tempfont.Size := Cell.TextFont.Size;
              tempfont.Style := Cell.TextFont.Style;
              tempfont.Color := Cell.TextFont.Color;

              if html.IsUrl then
              begin
                Cell.Url := html.Url;
                P.Url := html.Url;
              end
              else
                P.Url := '';

              if (html.HasImage) then
              begin
                alx := alx + cell.Bitmaps[html.ImgIdx].Width;
              end;

              if (html.Text <> '') then
              begin
                if cc <> 0 then
                begin
                  if Assigned(tempfont) then
                    tempfont.Free;

                  tempfont := TFont.Create;
                  tempfont.Name := Cell.HTML[cc-1].FontName;
                  tempfont.Size := Cell.HTML[cc-1].Size;
                  tempfont.Style := Cell.HTML[cc-1].FontStyle;
                  tempfont.Color := Cell.HTML[cc-1].Color;

                  alX := alX + Round(p.GetWidthText(Cell.HTML[cc-1].Text, tempfont) );

                  if (html.Text = ' ') and (fsBold in html.FontStyle) then
                    alX := alX + 2;

                  if (Pos(' ', html.Text) = 1) and (fsBold in html.FontStyle) then
                    alx := alX + 2;

                  if (Cell.HTML[cc-1].HasImage) then
                  begin
                    if Cell.HTML.Count > 2 then
                    begin
                      if Cell.Bitmaps.Count > 1 then
                        alX := alX + Cell.Bitmaps[Cell.HTML[cc-1].ImgIdx].Width - 12
                      else
                        alX := alX + Cell.Bitmaps[Cell.HTML[cc-1].ImgIdx].Width + 10;
                    end;
                  end;
                end;
              end;

              html2 := html;
              Cell.HTML[cc] := html2;

              if (html.Text <> '') then
              begin
                tempfont.Size:= html.Size;
                tempfont.Color := html.Color;
                tempfont.Name := html.FontName;
                tempfont.Style := html.FontStyle;
                P.BackgroundColor := html.BGColor;
                if IsNewLine then
                begin
                  alY := alY + Round((tempfont.Size * 1.5));
                  alX := PosX + 2;
                end;

                IsNewLine := html.Y = 1;

                P.AddTextLine(alX, alY + 3, html.Text, tempfont, alLeft, False, True, False);
                P.BackgroundColor := clNone;
              end;

              if Assigned(tempfont) then
                tempfont.Free;

              Inc(cc);
            end;
          {$ENDREGION}
          end
          else
          begin
          {$REGION 'TEXT'}
            /// Regular text

            M.Left := PosX + 3;
            P.Marges:= M;

            temp := p.PDFDocument.TextWidth;

            if not Cell.IsMerged then
              p.PDFDocument.TextWidth := PosX + (Table.Columns[II].Width);

            LTemp := '';
            if P.PDFDocument.TextOccurences(sLineBreak, Cell.Text) > 1 then
              alY := alY - Round(cell.TextFont.Size*1.5);

            while (Pos(sLineBreak, Cell.Text) > 0) do
            begin
              P.AddTextLine(alX, alY, Copy(Cell.Text, 0, Pos(sLineBreak, Cell.Text)-1), Cell.TextFont, alLeft, True, True, False);
              alY := alY + Round(cell.TextFont.Size*1.5);
              Cell.Text := StringReplace(Cell.Text, Copy(Cell.Text, 0, Pos(sLineBreak, Cell.Text)-1)+sLineBreak, '', []);
            end;
            while (Pos(AnsiChar(#13), Cell.Text) > 0) do
            begin
              P.AddTextLine(alX, alY, Copy(Cell.Text, 0, Pos(AnsiChar(#13), Cell.Text)-1), Cell.TextFont, alLeft, True, True, False);
              alY := alY + Round(cell.TextFont.Size*1.5);
              Cell.Text := StringReplace(Cell.Text, Copy(Cell.Text, 0, Pos(AnsiChar(#13), Cell.Text)-1)+sLineBreak, '', []);
            end;
            while (Pos(#$D, Cell.Text) > 0) do
            begin
              P.AddTextLine(alX, alY, Copy(Cell.Text, 0, Pos(#$D, Cell.Text)), Cell.TextFont, alLeft, True, True, False);
              alY := alY + Round(cell.TextFont.Size*1.5);
              Cell.Text := StringReplace(Cell.Text, Copy(Cell.Text, 0, Pos(#$D, Cell.Text)-1), '', []);
              Cell.Text := StringReplace(Cell.Text, #$D, '', []);
            end;
            while (Pos(#$A, Cell.Text) > 0) do
            begin
              P.AddTextLine(alX, alY, Copy(Cell.Text, 0, Pos(#$A, Cell.Text)), Cell.TextFont, alLeft, True, True, False);
              alY := alY + Round(cell.TextFont.Size*1.5);
              Cell.Text := StringReplace(Cell.Text, Copy(Cell.Text, 0, Pos(#$A, Cell.Text)-1), '', []);
              Cell.Text := StringReplace(Cell.Text, #$A, '', []);
            end;

            cb := '';
            if Cell.IsCheckbox then
            begin
              if Cell.HAlign = taRightJustify then alX := alX - 5;
              if Cell.Text <> '' then alX := alX + 15 else alx := alX - 15;

              cb := P.SetCheckboxInText(PosX + 5, alY, Cell.TextFont, Cell.Checked);

              case Cell.HAlign of
                taLeftJustify: ;
                taRightJustify: alX := alX - 5;
                taCenter: alX := alX + 10;
              end;
            end;

            if Cell.TextFont.Color = Cell.BackgroundColor then
              Cell.Text := '';

            if (Mcw > 0) or (Mch > 0) then
            begin
              if (Mcw > 0) and not (Mch > 0) then
                Cell.Text := P.FitText(Cell.Text, Cell.TextFont, Mcw, Table.Rows[JJ].Height);

              if not (Mcw > 0) and (Mch > 0) then
                Cell.Text := P.FitText(Cell.Text, Cell.TextFont, Table.Columns[II].Width, Mch);

              if (Mcw > 0) and (Mch > 0) then
                Cell.Text := P.FitText(Cell.Text, Cell.TextFont, Mcw, Mch);
            end
            else
              Cell.Text := P.FitText(Cell.Text, Cell.TextFont, Table.Columns[II].Width, Table.Rows[JJ].Height);


            if Cell.IsMerged then
            begin
              case Cell.HAlign of
                taLeftJustify: ;
                taRightJustify:
                   alX := alX - P.GetWidthText(Cell.Text, Cell.TextFont);
                taCenter:
                   alX := alX - Round(P.GetWidthText(Cell.Text, Cell.TextFont) / 2);
              end;
            end;

            if (not Cell.IsMerged) or ( isBaseCell ) then
            begin
              P.AddTextLine(alX+textAdvmnt, alY, Cell.Text, Cell.TextFont, alLeft, True, True, False);
            end;

            p.PDFDocument.TextWidth := temp;
          {$ENDREGION}
          end;
          textAdvmnt := 0;

          {$ENDREGION}

        {$ENDREGION}
        end;
      {$ENDREGION}
      end;

      P.PDFDocument.Pages.Add;
      P := P.PDFDocument.CurrentPage;
      P.Marges := Mtemp;
      P.PDFDocument.DoSetHeader;
      if Assigned(OnSetHeader) then
        OnSetHeader(Table.PDFPage, P.PDFDocument.Pages.Count - 1);
      P.PDFDocument.DoSetFooter;
      if Assigned(OnSetFooter) then
        OnSetFooter(Table.PDFPage, P.PDFDocument.Pages.Count - 1);
      PosY := P.PDFDocument.Height - Table.Y;
      PosX := Table.X;
      C := P.Content;
    end; //HReps
  end; //VReps

  M.Left := LM;
  P.Marges := M;

  P.Content := C;

  P.PDFDocument.Width := DW;

  Rws.Free;
  Cls.Free;
  P.PDFDocument.CurrentPage.Free;
end;


end.
