{***************************************************************************}
{ TPlanner PDF IO component                                                 }
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

unit AdvPlannerPDFIO;

{$I TMSDEFS.INC}

interface

uses
  Classes, Windows, Planner, AdvPDFLib, AdvPDFIO, Generics.Collections, Math, Graphics;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.0.1.0 : Improved : PDF rendering quality


type
  TAdvPlannerPDFIO = class(TAdvPDFIOComponent)
  private
    FPlanner: TPlanner;
    FScale: Single;
    procedure DrawHtml(PDF: TPDFDocument; X, Y: Integer; AText: string; AFont: TFont; PI: TPlannerItem; APageIndex: Integer);
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    function GetVersionNr: Integer; override;
    procedure GeneratePDF(AFileName: string); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Planner: TPlanner read FPlanner write FPlanner;
    property Scale: Single read FScale write FScale;
  end;

procedure Register;

implementation

uses
  SysUtils, Types, JPEG, StdCtrls
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

procedure Register;
begin
  RegisterComponents('TMS Planner',[TAdvPlannerPDFIO]);
end;

{ TAdvPlannerPDFIO }

procedure TAdvPlannerPDFIO.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AComponent = FPlanner) and (AOperation = opRemove) then
    FPlanner := nil;
end;

procedure TAdvPlannerPDFIO.GeneratePDF(AFileName: string);
var
  PDF: TPDFDocument;
  PI: TPlannerItem;
  M: TPdfMarges;
  C, R: Integer;
  I, II, X, Y, W, H: Integer;
  IOScale, Scale: Single;
  RestHeight, RestWidth, ColCount, RowCount, PageCounter, Collisions, CollisionIndex, Positions: Integer;
  IStart, IEnd, IPos, CHeight: Integer;
  ActiveStart, ActiveEnd, LMarge: Integer;
  IText: string;
  LineColor, BgColor, BGColorTo: TColor;
  IsNextCollision, IsHTML: Boolean;
  Cell: TPDFTableRowCellItem;
  HTML: THTMLRef;
  TmpFont, SFont, CFont, GCFont, IFont: TFont;
begin
  pdf := TPDFDocument.Create(Self);
  if PageLayout = plLandscape then
  begin
    PDF.Width := PageSize.Height;
    PDF.Height := PageSize.Width;
  end
  else
  begin
    PDF.Width := PageSize.Width;
    PDF.Height := PageSize.Height;
  end;

  PDF.ImagesList := Planner.PlannerImages;
  Cell := TPDFTableRowCellItem.Create(nil);

  // Set Marges
  M.Top := 15;
  M.Bottom:= 15;
  M.Left:= 15;
  M.Right:= 15;

  PDF.CurrentPage.Marges := m;

  PDF.Header := (Header);
  if Assigned(OnSetHeader) then
    OnSetHeader(PDF.CurrentPage, PDF.Pages.Count - 1);

  PDF.Footer := (Footer);
  if Assigned(OnSetFooter) then
    OnSetFooter(PDF.CurrentPage, PDF.Pages.Count - 1);

  PDF.MetaData := MetaData;

  // Set Marges
  M.Top := 15;
  M.Bottom:= 15;
  M.Left:= 15;
  M.Right:= 15;

  PDF.CurrentPage.Marges := M;

  RestHeight := PDF.Height - M.Top - M.Bottom;
  RestWidth := PDF.Width - M.Left - M.Right;
  RowCount := Planner.GridControl.RowCount;
  ColCount := Planner.GridControl.ColCount;

  IOScale := Self.Scale;
  Scale := 1;

  SFont := TFont.Create;
  CFont := TFont.Create; 
  IFont := TFont.Create;
  GCFont := TFont.Create;

  IsHTML := False;
  if Planner.Sidebar.Position = spTop then
  begin
  {$REGION 'HORIZONTAL'}
    X := M.Left;

    // Fit To Page
    if (IOScale = 1) and (Planner.PositionWidth = 0) then
    begin
      Scale := (PDF.Height - M.Top - M.Bottom) / Planner.Height;
    end;
    
    {$REGION 'Group Captions'}
    W := 0;
    if (Planner.Header.Visible) and (Planner.Header.Height > 0) then
    begin
      if Planner.PositionGroup > 0 then
      begin      
        Y := M.Top;
        Positions := Planner.Positions;
        for R := 0 to Positions do
        begin
          W := Planner.GridControl.ColWidths[0];
          H := Round(Planner.GridControl.RowHeights[R] * Scale);

          LineColor := Planner.Header.LineColor;
          BgColor := Planner.Header.Color;
          BGColorTo := Planner.Header.ColorTo;
          // draw the rectangle
          PDF.CurrentPage.DrawRectangle(X, Y, Y + H, W, LineColor, BgColor, BgColorTo, clNone, clNone, True, PDF.CurrentPage.ID);  
          // draw the text
          GCFont.Assign(Planner.Header.GroupFont);
          GCFont.Size := Round((Planner.Header.GroupFont.Size * Scale) + 1);
          if Planner.Sidebar.Visible then
          begin
            if R - 1 >= 0 then
            begin
              IsHTML := Pos('</', Planner.Header.GroupCaptions[R-1]) > 0;
              if IsHTML then
              begin
                DrawHtml(PDF, X, Y, Planner.Header.GroupCaptions[R-1], GCFont, nil, PDF.CurrentPage.ID);
              end
              else
                pdf.CurrentPage.AddTextLine(X + 2, Y + GCFont.Size, Planner.Header.GroupCaptions[R-1], GCFont);
            end;
          end
          else
            IsHTML := Pos('</', Planner.Header.GroupCaptions[R]) > 0;
            if IsHTML then
            begin
              DrawHtml(PDF, X, Y, Planner.Header.GroupCaptions[R], GCFont, nil, PDF.CurrentPage.ID);
            end
            else
              pdf.CurrentPage.AddTextLine(X + 2, Y + GCFont.Size, Planner.Header.GroupCaptions[R], GCFont);  

          Y := Y + H;
          RestHeight := RestHeight - H;
          if R + 1 <= Positions then
          begin
            if RestHeight - Planner.GridControl.RowHeights[R+1] <= 0 then
            begin
              PDF.Pages.Add;
              Y := M.Top;
              RestHeight := PDF.Height - M.Top - M.Bottom;
            end;
          end;
        end;  
        end;
        
      X := X + W;
      Dec(RestWidth, W);
    end;
    {$ENDREGION}
    
    {$REGION 'Captions'}
    if (Planner.Header.Visible) and (Planner.Header.Height > 0) then
    begin              
      RestHeight := PDF.Height - M.Top - M.Bottom;
      Y := M.Top;
      Positions := Planner.Positions;
      PageCounter := 0;
      for R := 0 to Positions do
      begin
        W := Planner.GridControl.ColWidths[0]; 
        if Planner.PositionGroup > 0 then
          W := Round(W / 2);
        H := Round(Planner.GridControl.RowHeights[R] * Scale);
                  
        LineColor := Planner.Header.LineColor;
        BgColor := Planner.Header.Color;
        BGColorTo := Planner.Header.ColorTo;
        // draw the rectangle
        PDF.Pages[PageCounter].DrawRectangle(X, Y, Y + H, W, LineColor, BgColor, BgColorTo, clNone, clNone, True, PageCounter);
        // draw the text
        CFont.Assign(Planner.Header.Font);
        CFont.Size := Round((Planner.Header.Font.Size * Scale) + 1);
        if (Planner.Sidebar.Visible) OR (not (Planner.Sidebar.Visible) AND (R <> 0)) then
        begin
          IsHTML := Pos('</', Planner.Header.Captions[R]) > 0;
          if IsHTML then
          begin
            DrawHtml(PDF, X, Y, Planner.Header.Captions[R], CFont, nil, PDF.CurrentPage.ID);
          end
          else
            pdf.Pages[PageCounter].AddTextLine(X + 2, Y + CFont.Size, Planner.Header.Captions[R], CFont);
        end;
        Y := Y + H;
        RestHeight := RestHeight - H;

        if R + 1 <= Positions then
        begin
          if RestHeight - Round(Planner.GridControl.RowHeights[R+1] * Scale) <= 0 then
          begin
            PDF.Pages.Add;
            Inc(PageCounter);
            Y := M.Top;
            RestHeight := PDF.Height - M.Top - M.Bottom;
          end;
        end;
      end;

      X := X + W;
      Dec(RestWidth, W);
    end;
    R := 0;
    {$ENDREGION}

    {$REGION 'Grid Structure'}
    for C := 0 to ColCount - 1 do
    begin
      Y := M.Top;
      W := Planner.GridControl.ColWidths[C];

      LineColor := Planner.GridLineColor;
      ActiveStart := Planner.Display.ActiveStart;
      ActiveEnd := Planner.Display.ActiveEnd;

      if (C > ActiveStart - 1) and (C <= ActiveEnd - 1) then
        BgColor := Planner.Display.ColorActive
      else
        BgColor := Planner.Display.ColorNonActive;
      BGColorTo := clNone;

      for R := 0 to RowCount - 1 do
      begin
        H := Round(Planner.GridControl.RowHeights[R] * Scale);
        PDF.CurrentPage.DrawRectangle(X, Y, Y + H, W, LineColor, BgColor, BGColorTo, clNone, clNone, True, PDF.Pages.Count - 1);

        Y := Y + H;
      end;
      R := 0;

      X := X + W;
      Dec(RestWidth, W);

      if (RestWidth - W <= 0) and (C < ColCount - 1) then
      begin
        pdf.Pages.Add;
        RestWidth := PDF.Width - M.Left - M.Right;
        X := M.Left;
      end;
    end;
    {$ENDREGION}

    {$REGION 'SideBar'}
    RestWidth := PDF.Width - M.Left - M.Right;
    PageCounter := 0;
    if (Planner.Sidebar.Visible) and (Planner.Sidebar.Width > 0) then
    begin
      Y := M.Top;
      X := M.Left;
      if (Planner.Header.Visible) and (Planner.Header.Height > 0) then
      begin
        X := X + Planner.GridControl.ColWidths[0];
        Dec(RestWidth, Planner.GridControl.ColWidths[0]);
      end;
      
      H := Round(Planner.Sidebar.Width * Scale);

      for C := 0 to ColCount - 1 do
      begin
        // Draw the rectangle
        W := Planner.GridControl.ColWidths[C];
        LineColor := Planner.Sidebar.LineColor;
        BgColor := Planner.Sidebar.Background;
        BGColorTo := Planner.Sidebar.BackgroundTo;
        pdf.CurrentPage.DrawRectangle(X, Y, Y + H, W, LineColor, BgColor, BgColorTo, clNone, clNone, True, PageCounter);
        // draw the text
        SFont.Assign(Planner.Sidebar.Font);
        SFont.Size := Round((Planner.Sidebar.Font.Size * Scale) + 1);
        PDF.Pages[PageCounter].AddTextLine(X + 2, Y + SFont.Size, PlannerGetIdCol(Planner, C, R), SFont);

        X := X + W;
        Dec(RestWidth, W);

        if RestWidth - W <= 0 then
        begin
          Inc(PageCounter);
          X := M.Left;
          Y := M.Top;
          RestWidth := PDF.Width - M.Left - M.Right;
        end;
      end;
    end;
    {$ENDREGION}

    {$REGION 'Items'}
    for I := 0 to Planner.Items.Count - 1 do
    begin
      PageCounter := 0;
      RestWidth := PDF.Width - M.Left - M.Right;
      X := M.Left;
      Y := M.Top;

      PI := Planner.Items[I];
      IStart := PI.ItemBegin;
      IEnd := PI.ItemEnd;
      IPos := PI.ItemPos;
      CHeight := Round((PI.CaptionFont.Size * Scale) + 1 ) + 2;

      Collisions := 0;
      CollisionIndex := 1;
      for II := 0 to Planner.Items.Count - 1 do
      begin
        if (Planner.Items[II].ItemBegin = IStart) and (Planner.Items[II].ItemPos = IPos) then
        begin
          Inc(Collisions);
          IsNextCollision := (II < I);
          if IsNextCollision then
            Inc(CollisionIndex);
        end;
      end;

      if Collisions = 0 then
        CollisionIndex := 1;

      if (not Planner.Header.Visible) then
      begin
        Dec(IStart);
        Dec(IEnd);
      end;

      for C := 0 to IStart do
      begin
        X := X + Planner.GridControl.ColWidths[C];

        if X > RestWidth then
        begin
          while X > RestWidth do
          begin
            Inc(PageCounter);
            X := M.Left + Planner.GridControl.ColWidths[C];
            X := X - Planner.GridControl.ColWidths[0];
          end;
        end;
      end;

      W := 0;
      for C := IStart to IEnd - 1 do
      begin
        W := W + Planner.GridControl.ColWidths[C];
      end;

      for R := 0 to IPos do
      begin
        Y := Y + Round(Planner.GridControl.RowHeights[R] * Scale);
      end;

      H := Round(Planner.GridControl.RowHeights[IPos + 1] / Collisions) - Planner.ItemGap;
      H := Round(H * Scale);
      if (Collisions > 1) and (CollisionIndex > 1) then
        Y := Y + ((H + Round(Planner.ItemGap * Scale)) * (CollisionIndex - 1));

      // Draw the Caption
      LineColor := PI.BorderColor;
      BgColor := PI.CaptionBkg;
      BGColorTo := PI.CaptionBkgTo;
      // draw the rectangle
      if X + W - 2 > RestWidth then
      begin
        PDF.CurrentPage.DrawRectangle(X, Y, Y + CHeight, (RestWidth - X) + 2, LineColor, BgColor, BgColorTo, clNone, clNone, PI.CaptionBkgDirection = gdHorizontal, PageCounter);
        PDF.CurrentPage.DrawRectangle(M.Left, Y, Y + CHeight, W -(RestWidth - X) - 2, LineColor, BgColor, BgColorTo, clNone, clNone, PI.CaptionBkgDirection = gdHorizontal, PageCounter + 1);
      end
      else
        PDF.CurrentPage.DrawRectangle(X, Y, Y + CHeight, W, LineColor, BgColor, BgColorTo, clNone, clNone, PI.CaptionBkgDirection = gdHorizontal, PageCounter);
      // draw the text
      IFont.Assign(PI.CaptionFont);
      IFont.Size := Round((PI.CaptionFont.Size * Scale) + 1);
      IsHTML := Pos('</', PI.CaptionText) > 0;
      if IsHTML then
      begin
        DrawHtml(PDF, X, Y, PI.CaptionText, IFont, PI, PageCounter);
      end
      else
      begin
        case PI.CaptionType of
            ctNone: PI.CaptionText := '';
            ctText: ;
            ctTime: PI.CaptionText := PlannerGetIdCol(Planner, PI.ItemBegin, PI.ItemPos) + ' - ' + PlannerGetIdCol(Planner, PI.ItemEnd, PI.ItemPos);
            ctTimeText: PI.CaptionText := PlannerGetIdCol(Planner, PI.ItemBegin, PI.ItemPos) + ' - ' + PlannerGetIdCol(Planner, PI.ItemEnd, PI.ItemPos) + ' ' + PI.CaptionText;
        end;
        pdf.Pages[PageCounter].AddTextLine(X + 2, Y + IFont.Size, PI.CaptionText, IFont);
      end;

      Y := Y + CHeight;

      // Draw the Item
      BgColor := PI.Color;
      BGColorTo := PI.ColorTo;
      // draw the rectangle
      if X + W - 2 > RestWidth then
      begin
        PDF.CurrentPage.DrawRectangle(X, Y, Y + H - 2, (RestWidth - X) + 2, LineColor, BgColor, BgColorTo, BgColor, BgColorTo, True, PageCounter);
        PDF.CurrentPage.DrawRectangle(M.Left, Y, Y + H - 2, W - (RestWidth - X) - 2, LineColor, BgColor, BgColorTo, BgColor, BgColorTo, True, PageCounter + 1);
      end
      else
        PDF.CurrentPage.DrawRectangle(X, Y, Y + H - 2, W, LineColor, BgColor, BgColorTo, BgColor, BgColorTo, True, PageCounter);
      // draw the text
      IFont.Assign(PI.Font);
      IFont.Size := Round((PI.Font.Size * Scale) + 1);
      LMarge := M.Left;
      PDF.SetTextWidth(X + W );
      M.Left := X + 2;
      PDF.CurrentPage.Marges := M;
      
      for II := 0 to PI.Text.Count - 1 do
      begin
        IText := PI.Text[II];
        IsHTML := Pos('</', IText) > 0;
        if IsHTML then
        begin
          DrawHtml(PDF, X, Y, IText, IFont, PI, PageCounter);
        end
        else
        begin
          pdf.Pages[PageCounter].AddTextLine(X + 2, Y + IFont.Size, IText, IFont, TPDFALIGNMENT.alLeft, True);
        end;
        
        Y := PDF.LineYPosition + Round(IFont.Size / 2);
      end;            

      M.Left := LMarge;
      pdf.CurrentPage.Marges := M;
      PDF.SetTextWidth(pdf.Width - M.Left - M.Right);

    end;
    {$ENDREGION}
  {$ENDREGION}
  end
  else
  begin
  {$REGION 'VERTICAL'}
    X := M.Left;
    Y := M.Top;  
    
    // Fit To Page
    if (IOScale = 1) and (Planner.PositionWidth = 0) then
    begin
      Scale := (PDF.Width - M.Left - M.Right) / Planner.Width;
    end;

    {$REGION 'Group Captions'}
    if (Planner.Header.Visible) and (Planner.Header.Height > 0) then
    begin
      if Planner.PositionGroup > 0 then
      begin
        H := 0;           
        GCFont.Assign(Planner.Header.GroupFont);
        GCFont.Size := Round((Planner.Header.GroupFont.Size * Scale) * 1.5);
        for C := 0 to Planner.Positions do
        begin
          W := Planner.GridControl.ColWidths[C];
          W := Round(W * Scale);
          H := Round(Planner.GridControl.RowHeights[0] / 2);

          LineColor := Planner.Header.LineColor;
          BgColor := Planner.Header.Color;
          BGColorTo := Planner.Header.ColorTo;
          // draw the rectangle
          PDF.CurrentPage.DrawRectangle(X, Y, Y + H, W, LineColor, BgColor, BgColorTo, clNone, clNone, True, PDF.CurrentPage.ID);  
          // draw the text
          if Planner.Sidebar.Visible then
          begin
            if C - 1 >= 0 then
            begin
              IsHTML := Pos('</', Planner.Header.GroupCaptions[C-1]) > 0;
              if IsHTML then
              begin
                DrawHtml(PDF, X, Y, Planner.Header.GroupCaptions[C-1], GCFont, nil, PDF.CurrentPage.ID);
              end
              else
                pdf.CurrentPage.AddTextLine(X + 2, Y + GCFont.Size, Planner.Header.GroupCaptions[C-1], GCFont);
            end;
          end
          else
            IsHTML := Pos('</', Planner.Header.GroupCaptions[C]) > 0;
            if IsHTML then
            begin
              DrawHtml(PDF, X, Y, Planner.Header.GroupCaptions[C], GCFont, nil, PDF.CurrentPage.ID);
            end
            else
              pdf.CurrentPage.AddTextLine(X + 2, Y + GCFont.Size, Planner.Header.GroupCaptions[C], Planner.Header.GroupFont);  
          X := X + W;
        end;

        Y := Y + H;
        RestHeight := RestHeight - H;
      end;
    end;
    {$ENDREGION}

    {$REGION 'Captions'}
    if (Planner.Header.Visible) and (Planner.Header.Height > 0) then
    begin
      X := M.Left;
      Positions := Planner.Positions;
      if (Planner.Sidebar.Visible) AND (Planner.SideBar.Position = spLeftRight) then
        Inc(Positions);
      H := 0;          
      CFont.Assign(Planner.Header.Font);
      CFont.Size := Round((Planner.Header.Font.Size * Scale) * 1.5);
      for C := 0 to Positions do
        begin
          W := Planner.GridControl.ColWidths[C];
          W := Round(W * Scale);
          H := Planner.GridControl.RowHeights[0];
          if Planner.PositionGroup > 0 then
            H := Round(H / 2);
                       
          LineColor := Planner.Header.LineColor;
          BgColor := Planner.Header.Color;
          BGColorTo := Planner.Header.ColorTo;
          // draw the rectangle
          PDF.CurrentPage.DrawRectangle(X, Y, Y + H, W, LineColor, BgColor, BgColorTo, clNone, clNone, True, PDF.CurrentPage.ID);
          // draw the text
          if Planner.Sidebar.Visible then
          begin
            IsHTML := Pos('</', Planner.Header.Captions[C]) > 0;
            if IsHTML then
            begin
              DrawHtml(PDF, X, Y, Planner.Header.Captions[C], CFont, nil, PDF.CurrentPage.ID);
            end
            else
              pdf.CurrentPage.AddTextLine(X + 2, Y + CFont.Size, Planner.Header.Captions[C], CFont)
          end
          else
          begin
            if C + 1 <= Planner.Header.Captions.Count then
            begin
              IsHTML := Pos('</', Planner.Header.Captions[C + 1]) > 0;
              if IsHTML then
              begin
                DrawHtml(PDF, X, Y, Planner.Header.Captions[C + 1], CFont, nil, PDF.CurrentPage.ID);
              end
              else
                pdf.CurrentPage.AddTextLine(X + 2, Y + CFont.Size, Planner.Header.Captions[C + 1], CFont)                
            end;
          end;
          X := X + W;
        end;

      Y := Y + H;
      RestHeight := RestHeight - H;
    end;
    {$ENDREGION}

    {$REGION 'Grid Structure'}
    for R := Planner.GridControl.RowCount - RowCount to RowCount - 1 do
    begin
      X := M.Left;
      if (Planner.Sidebar.Visible) and (Planner.Sidebar.Width > 0) AND ((Planner.Sidebar.Position = spLeft) OR (Planner.Sidebar.Position = spLeftRight)) then
        X := X + Round(Planner.Sidebar.Width * Scale);
      H := Planner.GridControl.RowHeights[R];

      if (Planner.Sidebar.Visible) and (Planner.Sidebar.Width > 0) AND ((Planner.Sidebar.Position = spLeft) OR (Planner.Sidebar.Position = spLeftRight)) then
        C := 1
      else
        C := 0;

      while C <= Planner.Positions do
      begin
        W := Round(Planner.GridControl.ColWidths[C] * Scale);

        LineColor := Planner.GridLineColor;
        ActiveStart := Planner.Display.ActiveStart;
        ActiveEnd := Planner.Display.ActiveEnd;
        if Planner.Header.Visible then
        begin
          Dec(ActiveStart);
          Dec(ActiveEnd);
        end;
        if (R > ActiveStart) and (R <= ActiveEnd) then
          BgColor := Planner.Display.ColorActive
        else
          BgColor := Planner.Display.ColorNonActive;
        BGColorTo := BgColor;

        PDF.CurrentPage.DrawRectangle(X, Y, Y + H, W, LineColor, BgColor, BGColorTo, clNone, clNone, True, PDF.CurrentPage.ID);

        X := X + W;

        Inc(C);
      end;

      Y := Y + H;
      RestHeight := RestHeight - H;

      if RestHeight - H <= 0 then
      begin
        PDF.Pages.Add;
        Y := M.Top;
        RestHeight := PDF.Height - M.Top - M.Bottom;
      end;
    end;
    {$ENDREGION}

    {$REGION 'SideBar'}
    if (Planner.Sidebar.Visible) and (Planner.Sidebar.Width > 0) then
    begin
      X := M.Left;
      Y := M.Top;
      if Planner.Header.Visible then
        Y := Y + Planner.GridControl.RowHeights[0];

      if (Planner.Sidebar.Position = spRight) then
      begin                                   
        for C := 0 to Planner.Positions - 1 do
        begin
          X := X + Round(Planner.GridControl.ColWidths[C] * Scale);
        end;
      end;
          
      RestHeight := PDF.Height - M.Top - M.Bottom; 
      if Planner.Header.Visible then     
        RestHeight := RestHeight - Planner.GridControl.RowHeights[0];
      PageCounter := 0;

      C := 0;
      for R := 0 to RowCount - 1 do
      begin
        W := Round(Planner.Sidebar.Width * Scale);
        H := Planner.GridControl.RowHeights[0];
        LineColor := Planner.Sidebar.LineColor;
        BgColor := Planner.Sidebar.Background;
        BGColorTo := Planner.Sidebar.BackgroundTo;
        // draw the rectangle
        PDF.Pages[PageCounter].DrawRectangle(X, Y, Y + H, W, LineColor, BgColor, BgColorTo, clNone, clNone, False, PageCounter);
        // draw the text
        SFont.Assign(Planner.Sidebar.Font);
        SFont.Size := Round((Planner.Sidebar.Font.Size * Scale) * 1.5);
        PDF.Pages[PageCounter].AddTextLine(X + 2, Y + SFont.Size, PlannerGetTimeSlotText(Planner, R, C), SFont);

        Y := Y + H;
        RestHeight := RestHeight - H;

        if RestHeight - H < 0 then
        begin
          Inc(PageCounter);
          Y := M.Top;
          RestHeight := PDF.Height - M.Top - M.Bottom;
        end;
      end;

      if (Planner.Sidebar.Position = spLeftRight) then
      begin
        X := M.Left;
        Y := M.Top;
        RestHeight := PDF.Height - M.Top - M.Bottom - Planner.Header.Height;
        if Planner.Header.Visible then
          Y := Y + Planner.GridControl.RowHeights[0];

        for C := 0 to Planner.Positions do
        begin
          X := X + Round(Planner.GridControl.ColWidths[C] * Scale);
        end;  

        C := 0;
        PageCounter := 0;
        for R := 0 to RowCount - 1 do
        begin
          W := Round(Planner.Sidebar.Width * Scale);
          H := Planner.GridControl.RowHeights[0];
          LineColor := Planner.Sidebar.LineColor;
          BgColor := Planner.Sidebar.Background;
          BGColorTo := Planner.Sidebar.BackgroundTo;
          // draw the rectangle
          PDF.Pages[PageCounter].DrawRectangle(X, Y, Y + H, W, LineColor, BgColor, BgColorTo, clNone, clNone, False, PageCounter);
          // draw the text
          SFont.Assign(Planner.Sidebar.Font);
          SFont.Size := Round((Planner.Sidebar.Font.Size * Scale)  * 1.5 );
          PDF.Pages[PageCounter].AddTextLine(X + 2, Y + SFont.Size, PlannerGetIdCol(Planner, R, C), SFont);

          Y := Y + H;
          RestHeight := RestHeight - H;

          if RestHeight - H < 0 then
          begin
            Inc(PageCounter);
            Y := M.Top;
            RestHeight := PDF.Height - M.Top - M.Bottom;
          end;
        end;
      end;
    end;
    {$ENDREGION}

    {$REGION 'Items'}
      for I := 0 to Planner.Items.Count - 1 do
      begin
        RestHeight := PDF.Height - M.Top - M.Bottom;
        X := M.Left + 2;
        Y := M.Top;
        PI := Planner.Items[I];
        IStart := PI.ItemBegin;
        IEnd := PI.ItemEnd;
        IPos := PI.ItemPos;
        CHeight := Round((PI.CaptionFont.Size * Scale) * 1.5 )+ 2;

        Collisions := 0;
        CollisionIndex := 1;
        for II := 0 to Planner.Items.Count - 1 do
        begin
          if (Planner.Items[II].ItemBegin = IStart) and (Planner.Items[II].ItemPos = IPos) then
          begin
            Inc(Collisions);
            IsNextCollision := (II < I);
            if IsNextCollision then
              Inc(CollisionIndex);
          end;
        end;

        if Collisions = 0 then
          CollisionIndex := 1;

        if (Planner.Sidebar.Visible) and (Planner.Sidebar.Width > 0) AND ((Planner.Sidebar.Position = spLeft) OR (Planner.Sidebar.Position = spLeftRight)) then
        begin
          IPos := IPos + 1;
        end;

        if (Planner.Header.Visible) and (Planner.Header.Height > 0) then
          Inc(IStart);

        PageCounter := 0;
        for R := 0 to IStart - 1 do
        begin
          Y := Y + Planner.GridControl.RowHeights[R];

          if Y > RestHeight then
          begin
            Inc(PageCounter);
            Y := M.Top;
            if PageSize.PageSize = psA4 then
              Y := Y + Planner.GridControl.RowHeights[R];
          end;
        end;

        H := 0;
        for R := IStart - 1 to IEnd - 1 do
        begin
          H := H + Planner.GridControl.RowHeights[R];
        end;

        if not Planner.Header.Visible then
          H := H - Planner.GridControl.RowHeights[0]
        else
          H := H - CHeight;

        if (Planner.Sidebar.Visible) and (Planner.Sidebar.Width > 0) AND ((Planner.Sidebar.Position = spLeft) OR (Planner.Sidebar.Position = spLeftRight)) then
        begin
          C := 0;
          while C < IPos do
          begin
            X := X + Round(Planner.GridControl.ColWidths[C] * Scale);
            Inc(C);
          end;
        end
        else
        begin
          C := 0;
          while C < IPos + 1 do
          begin
            if C <> 0 then
              X := X + Round(Planner.GridControl.ColWidths[C] * Scale);
            Inc(C);
          end;
        end;

        if NOT Planner.Sidebar.Visible then
          Inc(IPos);

        W := Round(Planner.GridControl.ColWidths[IPos] / Collisions) - Planner.ItemGap;
        W := Round(W * Scale);

        if (Collisions > 1) and (CollisionIndex > 1) then
          X := X + ((W + Round(Planner.ItemGap * Scale)) * (CollisionIndex - 1));

        // Draw the Caption
        LineColor := PI.BorderColor;
        BgColor := PI.CaptionBkg;
        BGColorTo := PI.CaptionBkgTo;
        // draw the rectangle
        if RestHeight - (Y + CHeight) < 0 then
        begin
          PDF.CurrentPage.DrawRectangle(X, M.Top, M.Top + CHeight, W, LineColor, BgColor, BgColorTo, BgColor, BgColorTo, PI.CaptionBkgDirection = gdHorizontal, PageCounter + 1);
        end
        else
          PDF.CurrentPage.DrawRectangle(X, Y, Y + CHeight, W, LineColor, BgColor, BgColorTo, BgColor, BgColorTo, PI.CaptionBkgDirection = gdHorizontal, PageCounter);
        // draw the text
        IFont.Assign(PI.CaptionFont);
        IFont.Size := Round((PI.CaptionFont.Size * Scale) * 1.5);
        IsHTML := Pos('</', PI.CaptionText) > 0;
        if IsHTML then
        begin
          DrawHtml(PDF, X, Y, PI.CaptionText, IFont, PI, PageCounter);
        end
        else
        begin
          case PI.CaptionType of
              ctNone: PI.CaptionText := '';
              ctText: ;
              ctTime: PI.CaptionText := PlannerGetIdCol(Planner, PI.ItemBegin, PI.ItemPos) + ' - ' + PlannerGetIdCol(Planner, PI.ItemEnd, PI.ItemPos);
              ctTimeText: PI.CaptionText := PlannerGetIdCol(Planner, PI.ItemBegin, PI.ItemPos) + ' - ' + PlannerGetIdCol(Planner, PI.ItemEnd, PI.ItemPos) + ' ' + PI.CaptionText;
          end;
          pdf.Pages[PageCounter].AddTextLine(X + 2, Y + IFont.Size, PI.CaptionText, IFont);
        end;

        Dec(RestHeight, CHeight);

        Y := Y + CHeight;

        // Draw the Item
        BgColor := PI.Color;
        BGColorTo := PI.ColorTo;
        // draw the rectangle
        if RestHeight - (Y + H) < 0 then
        begin
          RestHeight := pdf.Height - M.Top - M.Bottom;
          PDF.CurrentPage.DrawRectangle(X, Y, RestHeight - 2, W, LineColor, BgColor, BgColorTo, BgColor, BgColorTo, True, PageCounter);
          H := (Y + H + CHeight) - RestHeight - 2;
          PDF.CurrentPage.DrawRectangle(X, M.Top, H, W, LineColor, BgColor, BgColorTo, BgColor, BgColorTo, True, PageCounter + 1);
        end
        else
          PDF.CurrentPage.DrawRectangle(X, Y, Y + H - 2, W, LineColor, BgColor, BgColorTo, BgColor, BgColorTo, True, PageCounter);

        // draw the text
        LMarge := M.Left;           
        PDF.SetTextWidth(X + W - LMarge);
        M.Left := X + 2;
        PDF.CurrentPage.Marges := M;
        IFont.Assign(PI.Font);
        IFont.Size := Round((PI.Font.Size * Scale) * 1.5);
        for II := 0 to PI.Text.Count - 1 do
        begin       
          IText := PI.Text[II] ;
          IsHTML := Pos('</', IText) > 0;
          if IsHTML then
          begin
            DrawHtml(PDF, X, Y, IText, IFont, PI, PageCounter);
          end
          else
          begin
            IText := PDF.CurrentPage.FitText(IText, IFont, W, H);
            pdf.Pages[PageCounter].AddTextLine(X + 2, Y + IFont.Size, IText, IFont, TPDFALIGNMENT.alLeft, True);
          end;
          
          Y := PDF.LineYPosition + Round(IFont.Size / 2);
        end;

        M.Left := LMarge;
        pdf.CurrentPage.Marges := M;
        PDF.SetTextWidth(pdf.Width - M.Left - M.Right);
      end;
      {$ENDREGION}

  {$ENDREGION}

  end;

  SFont.Free;
  CFont.Free;
  IFont.Free;
  GCFont.Free;
  Cell.Free;
  PDF.GeneratePDF(AFileName);
  PDF.Free;
end;

function TAdvPlannerPDFIO.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

constructor TAdvPlannerPDFIO.Create(AOwner: TComponent);
begin
  inherited;
  FScale := 1;
end;

procedure TAdvPlannerPDFIO.DrawHtml(PDF: TPDFDocument; X, Y: Integer; AText: string; AFont: TFont; PI: TPlannerItem; APageIndex: Integer);
var
  Cell: TPDFTableRowCellItem;
  HTML: THTMLRef;
  TmpFont: TFont;
begin
  Cell := TPDFTableRowCellItem.Create(nil);
  Cell.TextFont.Assign(AFont);
  PDF.Pages[APageIndex].ConvertHTMLLine(AText, True, Cell);
  TmpFont := TFont.Create;
  TmpFont.Assign(AFont);
  for HTML in Cell.HTML do
  begin
    if HTML.Text <> '' then 
    begin           
      TmpFont.Name := HTML.FontName;
      TmpFont.Style := HTML.FontStyle;  
      TmpFont.Color := HTML.Color;
              
      if HTML.IsUrl then
      begin
        PDF.Pages[APageIndex].Url := HTML.Url;
        TmpFont.Color := Planner.URLColor;
      end
      else
        pdf.Pages[APageIndex].Url := '';
              
      pdf.Pages[APageIndex].BackgroundColor := HTML.BGColor;
      if HTML.Text = #$D#$A then
        PDF.Pages[APageIndex].AddNewLine
      else
        PDF.Pages[APageIndex].AddTextLine(X + 2, Y + TmpFont.Size, html.Text, TmpFont, alLeft, False, True, False); 
      PDF.Pages[APageIndex].BackgroundColor := clNone;
    end;
  end;
  TmpFont.Free;
  Cell.Free;
end;

end.
