//************************************************************************************//
//  File created automatically by GenerateRecords.xls                                 //
//  Do not modify by hand                                                             //
//************************************************************************************//

  ptgExp: begin ParsedStack.Push(fmOpenArray+RPNToString(CellList.ArrayFormula(GetWord(RPN,tPos+1),GetWord(RPN,tPos+3)),14, CellList)+fmCloseArray);inc(tPos,4); StartFormula:=''; end;
  ptgTbl: begin ParsedStack.Push(fmOpenArray+fmStartFormula+fmTableText+fmOpenParen+GetTableText(CellList.TableFormula(GetWord(RPN,tPos+1),GetWord(RPN,tPos+3)))+fmCloseParen+fmCloseArray);inc(tPos,4); StartFormula:=''; end;
  ptgAdd: begin ParsedStack.Pop(s2); ParsedStack.Pop(s1); ParsedStack.Push(s1+ParsedStack.FmSpaces+fmPlus+s2); end;
  ptgSub: begin ParsedStack.Pop(s2); ParsedStack.Pop(s1); ParsedStack.Push(s1+ParsedStack.FmSpaces+fmMinus+s2); end;
  ptgMul: begin ParsedStack.Pop(s2); ParsedStack.Pop(s1); ParsedStack.Push(s1+ParsedStack.FmSpaces+fmMul+s2); end;
  ptgDiv: begin ParsedStack.Pop(s2); ParsedStack.Pop(s1); ParsedStack.Push(s1+ParsedStack.FmSpaces+fmDiv+s2); end;
  ptgPower: begin ParsedStack.Pop(s2); ParsedStack.Pop(s1); ParsedStack.Push(s1+ParsedStack.FmSpaces+fmPower+s2); end;
  ptgConcat: begin ParsedStack.Pop(s2); ParsedStack.Pop(s1); ParsedStack.Push(s1+ParsedStack.FmSpaces+fmAnd+s2); end;
  ptgLT: begin ParsedStack.Pop(s2); ParsedStack.Pop(s1); ParsedStack.Push(s1+ParsedStack.FmSpaces+'<'+s2); end;
  ptgLE: begin ParsedStack.Pop(s2); ParsedStack.Pop(s1); ParsedStack.Push(s1+ParsedStack.FmSpaces+'<='+s2); end;
  ptgEQ: begin ParsedStack.Pop(s2); ParsedStack.Pop(s1); ParsedStack.Push(s1+ParsedStack.FmSpaces+'='+s2); end;
  ptgGE: begin ParsedStack.Pop(s2); ParsedStack.Pop(s1); ParsedStack.Push(s1+ParsedStack.FmSpaces+'>='+s2); end;
  ptgGT: begin ParsedStack.Pop(s2); ParsedStack.Pop(s1); ParsedStack.Push(s1+ParsedStack.FmSpaces+'>'+s2); end;
  ptgNE: begin ParsedStack.Pop(s2); ParsedStack.Pop(s1); ParsedStack.Push(s1+ParsedStack.FmSpaces+'<>'+s2); end;
  ptgIsect: begin ParsedStack.Pop(s2); ParsedStack.Pop(s1); ParsedStack.Push(s1+ParsedStack.FmSpaces+fmIntersect+s2); end;
  ptgUnion: begin ParsedStack.Pop(s2); ParsedStack.Pop(s1); ParsedStack.Push(s1+ParsedStack.FmSpaces+fmUnion+s2); end;
  ptgRange: begin ParsedStack.Pop(s2); ParsedStack.Pop(s1); ParsedStack.Push(s1+ParsedStack.FmSpaces+fmRangeSep+s2); end;
  ptgUplus: begin ParsedStack.Pop(s1); ParsedStack.Push(ParsedStack.FmSpaces+fmPlus+s1); end;
  ptgUminus: begin ParsedStack.Pop(s1); ParsedStack.Push(ParsedStack.FmSpaces+fmMinus+s1); end;
  ptgPercent: begin ParsedStack.Pop(s1); ParsedStack.Push(s1+ParsedStack.FmSpaces+fmPercent); end;
  ptgParen: begin ParsedStack.Pop(s1); ParsedStack.Push(ParsedStack.FmPreSpaces+fmOpenParen+s1+ParsedStack.FmPostSpaces+fmCloseParen); end;
  ptgMissArg: begin ParsedStack.Push(ParsedStack.FmSpaces+''); inc(tpos,0); end;
  ptgStr: begin ParsedStack.Push(ParsedStack.FmSpaces+fmStr+GetString(false, RPN, tpos+1, sl)+fmStr); inc(tpos,sl); end;
  ptgAttr: begin ProcessAttr(RPN, tpos+1, AttrLen, ParsedStack); inc(tpos, AttrLen); end;
  ptgSheet: begin  end;
  ptgEndSheet: begin  end;
  ptgErr: begin ParsedStack.Push(ParsedStack.FmSpaces+GetErrorText(RPN[tpos+1])); inc(tpos,1); end;
  ptgBool: begin ParsedStack.Push(ParsedStack.FmSpaces+GetBoolText(RPN[tpos+1])); inc(tpos,1); end;
  ptgInt: begin ParsedStack.Push(ParsedStack.FmSpaces+IntToStr(GetWord(RPN, tpos+1))); inc(tpos,2); end;
  ptgNum: begin ParsedStack.Push(ParsedStack.FmSpaces+FmFloatToStr(GetDouble(RPN, tpos+1))); inc(tpos,8); end;
  ptgArray: begin ParsedStack.Push(ParsedStack.FmSpaces+GetArrayText(RPN, tpos+1, ArrayPos)); inc(tpos,7); end;
  ptgFunc: begin s3:=ParsedStack.FmSpaces+GetFuncName(RPN, tpos+1, np, np); if np>0 then ParsedStack.Pop(s2) else s2:=''; for i:=2 to np do begin ParsedStack.Pop(s1); s2:=s1+fmFunctionSep+s2; end; ParsedStack.Push( s3+fmOpenParen+s2+fmCloseParen); inc(tpos,2); end;

  ptgFuncVar:
    begin
      s3:=ParsedStack.FmSpaces+GetFuncNameVar(RPN, tpos+1, np, IsAddin);
      if np>0 then ParsedStack.Pop(s2) else s2:='';
      for i:=2 to np do
      begin
        ParsedStack.Pop(s1);
        s2:=s1+fmFunctionSep+s2;
      end;
      if IsAddin then
      begin
        ParsedStack.Pop(s4);
        s3:=s3 + s4;
      end;
      ParsedStack.Push( s3+fmOpenParen+s2+fmCloseParen); inc(tpos,3);
    end;

  ptgName: begin ParsedStack.Push(ParsedStack.FmSpaces+GetName(RPN, -1, tpos+1, CellList)); inc(tpos,4); end;
  ptgRef: begin ParsedStack.Push(ParsedStack.FmSpaces+GetRef(RPN, tpos+1)); inc(tpos,4); end;
  ptgArea: begin ParsedStack.Push(ParsedStack.FmSpaces+GetArea(RPN, tpos+1)); inc(tpos,8); end;
  ptgMemArea: begin ReadMemArea(RPN, tpos+1, ArrayPos); inc(tpos,6); end;
  ptgMemErr: begin ReadMemErr(RPN, tpos+1, ArrayPos); inc(tpos,6); end;
  ptgMemNoMem: begin  end;
  ptgMemFunc: begin inc(tpos, 2) end;
  ptgRefErr: begin ParsedStack.Push(ParsedStack.FmSpaces+fmErrRef); inc(tpos,4); end;
  ptgAreaErr: begin ParsedStack.Push(ParsedStack.FmSpaces+fmErrRef); inc(tpos,8); end;
  ptgRefN: begin ParsedStack.Push(ParsedStack.FmSpaces+GetRef(RPN, tpos+1)); inc(tpos,4); end;
  ptgAreaN: begin ParsedStack.Push(ParsedStack.FmSpaces+GetArea(RPN, tpos+1)); inc(tpos,8); end;
  ptgMemAreaN: begin ReadMemArea(RPN, tpos+1, ArrayPos); inc(tpos,6); end;
  ptgMemNoMemN: begin  end;
  ptgNameX: begin ParsedStack.Push(ParsedStack.FmSpaces+GetNameX(RPN, tpos+1, CellList)); inc(tpos,6); end;
  ptgRef3d: begin ParsedStack.Push(ParsedStack.FmSpaces+GetRef3D(RPN, tpos+1, CellList)); inc(tpos,6); end;
  ptgArea3d: begin ParsedStack.Push(ParsedStack.FmSpaces+GetArea3D(RPN, tpos+1, CellList)); inc(tpos,10); end;
  ptgRefErr3d: begin ParsedStack.Push(ParsedStack.FmSpaces+fmErrRef); inc(tpos,6); end;
  ptgAreaErr3d: begin ParsedStack.Push(ParsedStack.FmSpaces+fmErrRef); inc(tpos,10); end;
