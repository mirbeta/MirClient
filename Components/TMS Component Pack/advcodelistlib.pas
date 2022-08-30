{***************************************************************************}
{ TAdvMemo component                                                        }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2001 - 2013                                        }
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

{$I TMSDEFS.INC}

unit AdvCodeListLib;

interface

uses
  Windows,Graphics, Classes, SysUtils, Advmemo, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

procedure ExtractURL(s: string; var urls: TStringList);
procedure DrawCustomLine(ACanvas: TCanvas; Caption: String; var style: TStyle;DM: TDrawMode; PR: TRect; InternalStyles: TAdvCustomMemoStyler; Delimiters, NoStart, NoHex, tmpNo: String);

implementation

procedure ExtractURL(s: string; var urls: TStringList);
begin
  if not Assigned(urls) then
    Exit;
  urls.Clear;
  urls.Add(s);
end;

procedure DrawCustomLine(ACanvas: TCanvas; Caption: String; var style: TStyle;DM: TDrawMode; PR: TRect; InternalStyles: TAdvCustomMemoStyler; Delimiters, NoStart, NoHex, tmpNo: String);
var
  rct0, rct1, rct, lineRct: TRect;
  LineSelStart, LineSelEnd, posln, i: integer;
  urls: TStringList;
  S, S1, S2, S3: string;
  xSelStartX, xSelStartY, xSelEndX, xSelEndY: integer;
  isinlinecomment:boolean;
  backupstyle:Tstyle;
  backupstring:string;
  LineCanvas: TCanvas;
  lit: char;
  BackColor: TColor;

  function Equal(s1, s2: string): boolean;
  begin
    if false{FCaseSensitive} then
      Result := s1 = s2
    else
      Result := AnsiLowerCase(s1) = AnsiLowerCase(s2);
  end;

  //--------- FIND LINE SELECTION -------------
  procedure FindLineSelection(Selpart: string);
  var
    len: integer;
  begin
    s1 := '';
    s2 := '';
    s3 := '';

    len := length(Selpart);
    LineSelStart := 0;
    LineSelEnd := 0;

    if (xSelStartY = xSelEndY) then // single line selection
    begin
      if xSelStartX = xSelEndX then
      begin // nothing is selected
        s1 := Selpart;
        exit;
      end;
      if xSelStartX >= posln + len then // selection didn't start
      begin
        s1 := Selpart;
        Exit;
      end;
      if xSelEndX <= posln then // selection ended
      begin
        s3 := Selpart;
        Exit;
      end;
      LineSelStart := xSelStartX - posln;
      LineSelEnd := xSelEndX - posln;
    end;

    if LineSelEnd > len then LineSelEnd := len;
    if LineSelEnd < 0 then LineSelEnd := 0;
    if LineSelStart < 0 then LineSelStart := 0;
    if LineSelStart > len then LineSelStart := len;

    S1 := Copy(Selpart, 1, LineSelStart);
    S2 := Copy(Selpart, LineSelStart + 1, LineSelEnd - LineSelStart);
    S3 := Copy(Selpart, LineSelEnd + 1, len - LineSelEnd);
  end;

  //------------- DRAW PART ---------------------
  procedure DrawPart(Part: string; var Drawstyle: TStyle);
  var
    len, selcol: integer;

    procedure loadfromitemstyle;
    begin
      with LineCanvas do
      begin
        try
          Font.Color := InternalStyles.AllStyles.Items[DrawStyle.index].Font.Color;
          Font.Style := InternalStyles.AllStyles.Items[DrawStyle.index].Font.Style;
          Brush.Color := InternalStyles.AllStyles.Items[DrawStyle.index].BGColor;
        except
          on Exception do
          begin
            Font.Color := clBlack;//Self.Font.Color; // TODO:
            //Font.Style := Self.Font.Style;
            Brush.Color := BackColor; //TODO: Self.BkColor;
          end;
        end;
      end;
    end;

  begin
    len := Length(Part);

    if len > 0 then
    begin

      with LineCanvas do
      begin
        Font.Color := clBlack;//Self.Font.Color;   //TODO:
        Font.Style := [];//Self.Font.Style;
        Brush.Color := BackColor;// TODO: Self.BkColor;
        begin
          if (DrawStyle.isComment > 0) and (not DrawStyle.isURL) then
          begin
            Font.Color := InternalStyles.CommentStyle.TextColor;
            Font.Style := InternalStyles.CommentStyle.Style;
            Brush.Color := InternalStyles.CommentStyle.BkColor;
          end
          else
          begin
            if (DrawStyle.isBracket) and (not DrawStyle.isURL) then
              LoadFromItemStyle
            else
            begin
              if DrawStyle.isnumber then
              begin
                Font.Color := InternalStyles.NumberStyle.TextColor;
                Font.Style := InternalStyles.NumberStyle.Style;
                Brush.Color := InternalStyles.NumberStyle.BkColor;
              end;
              if DrawStyle.isdelimiter then loadfromitemstyle;
              if DrawStyle.iskeyWord then loadfromitemstyle;
              if DrawStyle.isURL then
              begin
                Font.Color := clBlack;//TODO: FUrlStyle.FTextColor;
                //Font.Style := FUrlStyle.Style;
                Brush.Color := clBlue;//TODO: FUrlStyle.FBkColor;
              end;
            end;
          end;
        end;

        if part <> '' then
        begin
          FindLineSelection(part);
          selcol := LineCanvas.Font.Color;
          if s1 <> '' then
          begin
            DrawText(LineCanvas.Handle, PChar(s1), length(s1), rct1,
              DT_LEFT or DT_SINGLELINE or DT_NOPREFIX or DT_NOCLIP);

            rct1.Left := rct1.Left + LineCanvas.TextWidth(s1);
          end;
          if s2 <> '' then
          begin
            DrawText(LineCanvas.Handle, PChar(s2), length(s2), rct1,
              DT_LEFT or DT_SINGLELINE or DT_NOPREFIX or DT_NOCLIP);
              rct1.Left := rct1.Left + LineCanvas.TextWidth(s2);
          end;
          if s3 <> '' then
          begin
            LineCanvas.Font.Color := selcol;
            LineCanvas.Brush.Style:= bsClear;
            DrawText(LineCanvas.Handle, PChar(s3), length(s3), rct1,
            DT_LEFT or DT_SINGLELINE or DT_NOPREFIX or DT_NOCLIP);
            rct1.Left := rct1.Left + LineCanvas.TextWidth(s3);;
          end;
          Inc(posln, length(Part));
        end;
      end;
    end;
  end;

  procedure BufferingDraw(part:string;var bufstyle: TStyle);

    function egalstyle(stl1,stl2:Tstyle): Boolean;
    begin
      Result :=
      (stl1.isComment       = stl2.isComment) and
      (stl1.isBracket       = stl2.isBracket) and
      (stl1.isnumber        = stl2.isnumber)  and
      (stl1.iskeyWord       = stl2.iskeyWord) and
      (stl1.isdelimiter     = stl2.isdelimiter) and
      (stl1.isURL           = stl2.isURL) and
      (stl1.EndBracket      = stl2.EndBracket) and
      (stl1.index           = stl2.index);
    end;

    procedure ResetPartStyle;
    begin
      bufstyle.isnumber := False;
      bufstyle.iskeyWord := False;
      bufstyle.isdelimiter := False;
      bufstyle.isURL := False;
    end;

  begin
    if egalstyle(bufstyle,backupstyle)  then
    begin
      backupstring := backupstring + part;
    end
    else
    begin
      DrawPart(backupstring,backupstyle);
      backupstyle := bufstyle;
      backupstring := part;
    end;
    resetPartStyle;
  end;

  //------------- DRAW SEGMENTS ---------------------
  procedure DrawSegments(S: string; var rct: TRect;
    var SegmentStyle: Tstyle);
  var
    i, j, len, toStart, toEnd, Innr, lc, rc: integer;
    done, WasPoint: boolean;
    validno: boolean;
    part: string;
    numsallowed:string;
  begin
    s := string(PChar(s));

    if not Assigned(InternalStyles) then
    begin
      BufferingDraw(s, SegmentStyle);
      Exit;
    end;

    toStart := 1;
    validno := True;
    done := false;
    while S <> '' do
    begin
      Len := Length(S);
      if (len = 0) or (tostart > len) then
        Exit;

      if not done then
      begin
        validno := (toStart = 1) or (s[toStart] = #32) or
          ((AnsiPos(S[toStart], Delimiters) > 0) or (Delimiters = ''));
      end;

      done := False;

      // Parse for multi-line comments
      if (not SegmentStyle.isBracket) then
      if (InternalStyles.MultiCommentLeft <> '') and
        (InternalStyles.MultiCommentRight <> '') then
      begin
        if (SegmentStyle.isComment > 0) then
        begin
          rc := AnsiPos(InternalStyles.MultiCommentRight, s);
          if (rc > 0) then
          begin
            BufferingDraw(copy(s, 1,
              rc + length(InternalStyles.MultiCommentRight) - 1), SegmentStyle);
            Delete(s, 1, rc + length(InternalStyles.MultiCommentRight) - 1);
            decz(SegmentStyle.isComment);
            len := length(s);
            if len = 0 then
              Exit;
          end
          else
          begin
            BufferingDraw(s, SegmentStyle);
            Exit;
          end;
        end
        else
        begin
          // rc := ansipos(InternalStyles.LineComment, s);
          // for canceling the multi-line comment
          lc := ansipos(InternalStyles.MultiCommentLeft, s);
          if (lc = tostart) {and ((lc < rc) or (rc = 0))} then
//          if (lc > 0) and ((lc < rc) or (rc = 0)) and (not SegmentStyle.isBracket) then
          begin
            //part := copy(s, 1, lc - 1);
            //BufferingDraw(part, SegmentStyle);
            Delete(s, 1, (lc - 1) + length(InternalStyles.MultiCommentLeft));
            inc(SegmentStyle.isComment);
            BufferingDraw(InternalStyles.MultiCommentLeft, SegmentStyle);
            len := length(s);
            if len = 0 then
              Exit;
            done := True;
          end
        end;
      end;

      if not done then
      begin
        // line comment
        if (SegmentStyle.isComment = 0)  then
        begin
          if (AnsiPos(InternalStyles.LineComment, s) = tostart) and (not SegmentStyle.isBracket) then
          begin
            part := copy(s, tostart, len - tostart + 1);
            inc(SegmentStyle.isComment);
            BufferingDraw(part, SegmentStyle);
            isinlinecomment := True;
            Exit;
          end;

          // parse for bracket
          if (SegmentStyle.isBracket) and (SegmentStyle.EndBracket <> #0) then
          begin
            // literal output

            if s[tostart] = lit then
            begin
              BufferingDraw(s[tostart], SegmentStyle);
              Delete(s, tostart, 1);
              len := length(s);
              if tostart > len then
                Exit;

              BufferingDraw(s[tostart], SegmentStyle);
              Delete(s, tostart, 1);
              len := length(s);
              if tostart > len then
                Exit;
              done := True;
              Continue;
            end;

            // end of bracket string detected here
            if s[tostart] = SegmentStyle.EndBracket then
            begin
              BufferingDraw(s[tostart], SegmentStyle);
              Delete(s, tostart, 1);
              SegmentStyle.isBracket := False;
              validno := False;
              done := True;
              Continue;
            end
            else
            begin
              BufferingDraw(s[tostart], SegmentStyle);
              inc(tostart);
              len := length(s);
              if tostart > len then
                Exit;
              done := True;
            end;
          end
          else
          begin
            SegmentStyle.EndBracket := #0;

            for lc := 0 to InternalStyles.AllStyles.Count - 1 do
            begin
              if InternalStyles.AllStyles.Items[lc].StyleType <> stBracket then
                Continue;

              SegmentStyle.EndBracket :=
                InternalStyles.AllStyles.Items[lc].BracketEnd;

              SegmentStyle.index := lc;
              if (SegmentStyle.EndBracket <> #0) and
                (s[toStart] = SegmentStyle.EndBracket) then
              begin
                SegmentStyle.isBracket := True;
                SegmentStyle.EndBracket := InternalStyles.AllStyles.Items[lc].BracketEnd;
                Break;
              end;
            end;

            if SegmentStyle.isBracket then
            begin
              BufferingDraw(s[toStart], SegmentStyle);
              Delete(s, toStart, 1);
              Continue;
            end;
          end;
        end;
      end; //End if not done

      len := length(s);
      if (Len = 0) or (toStart > len) then
        Exit;

      if not done then
        for i := 0 to InternalStyles.AllStyles.Count - 1 do
        begin
          if InternalStyles.AllStyles.Items[i].StyleType <> stSymbol then
            Continue;

          if (toStart <= len) and
             (AnsiPos(S[toStart], InternalStyles.AllStyles.Items[i].Symbols) > 0) then
          begin
            SegmentStyle.isDelimiter := True;
            SegmentStyle.index := i;
            BufferingDraw(s[toStart], SegmentStyle);
            Delete(s, toStart, 1);
            validno := True;
            Len := Length(S);
            done := True;
            Break;
          end;
        end;

      if done then
        Continue;

      toEnd := tostart;
      if (len = 0) or (tostart > Len) then
        Exit;

      if validno then
        if (AnsiPos(UpCase(S[tostart]),NoStart) > 0) then
        begin
          if pos(NoHex,Uppercase(s)) = toStart then
          begin
            numsallowed := tmpNo + 'ABCDEF';
            toEnd := toEnd + length(Nohex);
          end
          else
            numsallowed := tmpNo;

          WasPoint := False;
          Innr := toStart;

          while ((toEnd <= Len) and (AnsiPos(UpCase(S[toEnd]),numsallowed) > 0))  do
          begin
            if UpperCase(copy(s,tostart,toend)) = NoHex then
              numsallowed := tmpNo + 'ABCDEF';

            if S[toEnd] = '.' then
            begin
              if WasPoint then
              begin
                toEnd := Innr;
                Break;
              end;
              WasPoint := True;
              Innr := toEnd;
            end;
            Inc(toEnd);
          end;

          Dec(toEnd);

          if (tostart <= toend) then
          begin
            SegmentStyle.isDelimiter := False;
            SegmentStyle.isNumber := True;
            part := copy(s, tostart, toend - tostart + 1);
            Delete(s, tostart, toend - tostart + 1);
            BufferingDraw(part, SegmentStyle);
            validno := False;
            done := True;
          end;
        end;
      if done then continue;

      Len := Length(S);
      if (len = 0) or (tostart > Len) then
        Exit;

      toend := tostart;

      while (toend <= Len) and (S[toend] <> #32) and
        (AnsiPos(S[toend], delimiters) = 0) do
          Inc(toend);

      part := Copy(S, toStart, toEnd - toStart);
      if (part <> '') and (validno) then
        for i := 0 to InternalStyles.AllStyles.Count - 1 do
        begin
          if InternalStyles.AllStyles.Items[i].StyleType = stKeyword then
          begin
            if done then
              Break;

            for j := 0 to InternalStyles.AllStyles.Items[i].KeyWords.Count - 1 do
              if Equal(part, InternalStyles.AllStyles.Items[i].KeyWords.Strings[j]) then
              begin
                SegmentStyle.iskeyWord := True;
                SegmentStyle.index := i;

                if InternalStyles.CustomDraw then
                begin
                  BufferingDraw(backupstring,SegmentStyle);

                  InternalStyles.DrawKeyword(LineCanvas,part,rct1);
                  backupstring := '';
                end
                else
                  BufferingDraw(part, SegmentStyle);

                Delete(s, toStart, toend - tostart);
                done := True;
                Break;
              end;
          end;
        end;

      if done then
        Continue;

      if not done then
      begin
        BufferingDraw(s[toStart], SegmentStyle);
      end;
      inc(toStart);
    end;
  end;

begin
  rct0 := PR;
  LineRct := PR;
  rct1 := PR;

  LineCanvas := ACanvas;

  s := '';
  if Assigned(InternalStyles) then
     s := InternalStyles.Literal;

  if length(s) > 0 then
    lit := s[1]
  else
    lit := #0;

  S := Caption;
  BackColor:= clWhite;

  rct := rct0;
  posln := 0;

  urls := TStringList.Create;
  backupstyle := style;
  ExtractURL(s, urls);
  isinlinecomment := False;

  for i := 0 to urls.Count - 1 do
  begin
    style.isURL := False;
    DrawSegments(urls.Strings[i], rct1, style);
  end;

  urls.Free;

  DrawPart(BackupString,BackupStyle);

  if isinlinecomment then
    style.isComment := 0;
end;

end.
