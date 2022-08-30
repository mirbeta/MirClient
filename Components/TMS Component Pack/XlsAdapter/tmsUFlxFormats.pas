/// Definitions for the formats in a cell.
unit tmsUFlxFormats;
{$INCLUDE ..\FLXCOMPILER.INC}

interface
uses tmsUFlxMessages;
type

  /// <summary>
  /// Horizontal Alignment on a cell.
  /// </summary>
  THFlxAlignment=(
        /// <summary>
        /// General Alignment. (Text to the left, numbers to the right and Errors and booleans centered)
        /// </summary>
        fha_general,

        /// <summary>
        /// Aligned to the left.
        /// </summary>
        fha_left,

        /// <summary>
        /// Horizontally centered on the cell.
        /// </summary>
        fha_center,

        /// <summary>
        /// Aligned to the right.
        /// </summary>
        fha_right,

        /// <summary>
        /// Repeat the text to fill the cell width.
        /// </summary>
        fha_fill,

        /// <summary>
        /// Justify with spaces the text so it fills the cell width.
        /// </summary>
        fha_justify,

        /// <summary>
        /// Centered on a group of cells.
        /// </summary>
        fha_center_across_selection);


  /// <summary>
  /// Vertical Alignment on a cell.
  /// </summary>
  TVFlxAlignment=(
        /// <summary>
        /// Aligned to the top.
        /// </summary>
        fva_top,

        /// <summary>
        /// Vertically centered on the cell.
        /// </summary>
        fva_center,

        /// <summary>
        /// Aligned to the bottom.
        /// </summary>
        fva_bottom,

        /// <summary>
        /// Justified on the cell.
        /// </summary>
        fva_justify,

        /// <summary>
        /// Distributed on the cell.
        /// </summary>
        fva_distributed
        );

  /// <summary>
  /// Cell border style.
  /// </summary>
  TFlxBorderStyle= (
                      ///<summary>None</summary>
                      fbs_None,
                      ///<summary>Thin</summary>
                      fbs_Thin,
                      ///<summary>Medium</summary>
                      fbs_Medium,
                      ///<summary>Dashed</summary>
                      fbs_Dashed,
                      ///<summary>Dotted</summary>
                      fbs_Dotted,
                      ///<summary>Thick</summary>
                      fbs_Thick,
                      ///<summary>Double</summary>
                      fbs_Double,
                      ///<summary>Hair</summary>
                      fbs_Hair,
                      ///<summary>Medium dashed</summary>
                      fbs_Medium_dashed,
                      ///<summary>Dash dot</summary>
                      fbs_Dash_dot,
                      ///<summary>Medium_dash_dot</summary>
                      fbs_Medium_dash_dot,
                      ///<summary>Dash dot dot</summary>
                      fbs_Dash_dot_dot,
                      ///<summary>Medium dash dot dot</summary>
                      fbs_Medium_dash_dot_dot,
                      ///<summary>Slanted dash dot</summary>
                      fbs_Slanted_dash_dot
                      );


  const
    /// <summary>Automatic </summary>
    TFlxPatternStyle_Automatic = 0;

    /// <summary>None </summary>
    TFlxPatternStyle_None = 1;

    /// <summary>Solid </summary>
    TFlxPatternStyle_Solid = 2;

    /// <summary>Gray50 </summary>
    TFlxPatternStyle_Gray50 = 3;

    /// <summary>Gray75 </summary>
    TFlxPatternStyle_Gray75 = 4;

    /// <summary>Gray25 </summary>
    TFlxPatternStyle_Gray25 = 5;

    /// <summary>Horizontal </summary>
    TFlxPatternStyle_Horizontal = 6;

    /// <summary>Vertical </summary>
    TFlxPatternStyle_Vertical = 7;

    /// <summary>Down </summary>
    TFlxPatternStyle_Down = 8;

    /// <summary>Up </summary>
    TFlxPatternStyle_Up = 9;

    /// <summary>Diagonal hatch.</summary>
    TFlxPatternStyle_Checker = 10;

    /// <summary>bold diagonal.</summary>
    TFlxPatternStyle_SemiGray75 = 11;

    /// <summary>thin horz lines </summary>
    TFlxPatternStyle_LightHorizontal = 12;

    /// <summary>thin vert lines</summary>
    TFlxPatternStyle_LightVertical = 13;

    /// <summary>thin \ lines</summary>
    TFlxPatternStyle_LightDown = 14;

    /// <summary>thin / lines</summary>
    TFlxPatternStyle_LightUp = 15;

    /// <summary>thin horz hatch</summary>
    TFlxPatternStyle_Grid = 16;

    /// <summary>thin diag</summary>
    TFlxPatternStyle_CrissCross = 17;

    /// <summary>12.5 % gray</summary>
    TFlxPatternStyle_Gray16 = 18;

    /// <summary>6.25 % gray</summary>
    TFlxPatternStyle_Gray8 = 19;

type

  /// <summary>
  /// Pattern style for a cell. Use the constants TFlxPatternStyle_XXX to set this variable.
  /// </summary>
  TFlxPatternStyle=TFlxPatternStyle_Automatic..TFlxPatternStyle_Gray8;

  /// <summary>
  /// Defines a diagonal border for a cell.
  /// </summary>
  TFlxDiagonalBorder =(
     /// <summary> Cell doesn't have diagonal borders.</summary>
    fdb_None,

     /// <summary> Cell has a diagonal border going from top left to bottom right.</summary>
    fdb_DiagDown,

     /// <summary> Cell has a diagonal border going from top right to bottom left.</summary>
    fdb_DiagUp,

     /// <summary> Cell has both diagonal borders creating a cross.</summary>
    fdb_Both);

  /// <summary>
  /// Font style for a cell.
  /// </summary>
  TFlxFontStyle = (
    /// <summary>Font is bold.</summary>
    flsBold,

    /// <summary>Font is italic.</summary>
    flsItalic,

    /// <summary>Font is striked out.</summary>
    flsStrikeOut,

    /// <summary>Font is superscript.</summary>
    flsSuperscript,

    /// <summary>Font is subscript.</summary>
    flsSubscript);

  /// <summary>
  /// Types of underline you can make in an Excel cell. 
  /// </summary>                                        
  TFlxUnderline = (
    /// <summary>No underline.</summary>
    fu_None,

    /// <summary>Simple underline.</summary>
    fu_Single,

    /// <summary>Double underline.</summary>
    fu_Double,

    /// <summary>Simple underline but not underlining spaces between words.</summary>
    fu_SingleAccounting,

    /// <summary>Double underline but not underlining spaces between words.</summary>
    fu_DoubleAccounting);


  /// <summary>A set of TFlxFontStyle definitions.</summary>
  SetOfTFlxFontStyle= Set of TFlxFontStyle;

  /// <summary>this record describes an Excel font.</summary>
  TFlxFont=record
    /// <summary>Name of the font, like Arial or Times New Roman.</summary>
    Name: UTF16String;

    /// <summary>
    /// Height of the font (in units of 1/20th of a point). A Size20=200 means 10 points.
    /// </summary>
    Size20: Word;

    /// <summary>
    /// Index on the color palette.
    /// </summary>
    ColorIndex: integer;

    /// <summary>
    /// Style of the font, such as bold or italics. Underline is a different option.
    /// </summary>
    Style: SetOfTFlxFontStyle;

    /// <summary>
    /// Underline type.
    /// </summary>
    Underline: TFlxUnderline;

    /// <summary>
    /// Font family, (see Windows API LOGFONT structure).
    /// </summary>
    Family: byte;


    /// <summary>
    /// Character set. (see Windows API LOGFONT structure)
    /// </summary>
    CharSet: byte;
  end;

  /// <summary>
  /// Border style and color for one of the 4 sides of a cell.
  /// </summary>
  TFlxOneBorder=record

   /// <summary>
   /// Border style.
   /// </summary>
   Style: TFlxBorderStyle;

    /// <summary>
    /// Index to color palette.
    /// </summary>
    ColorIndex: integer;
  end;

  /// <summary>
  /// Defines the borders of a cell.
  /// </summary>                    
  TFlxBorders=record
    /// <summary> Left border.</summary>
    Left,

    /// <summary> Right border.</summary>
    Right,

    /// <summary> Top border.</summary>
    Top,

    /// <summary> Bottom border.</summary>
    Bottom,

    /// <summary> Diagonal borders.</summary>
    Diagonal: TFlxOneBorder;

    /// <summary> Style for the diagonal borders.</summary>
    DiagonalStyle: TFlxDiagonalBorder;
  end;

  /// <summary>
  /// Fill pattern and color for the background of a cell.
  /// </summary>
  TFlxFillPattern=record

    /// <summary>
    /// Fill style.
    /// </summary>
    Pattern: TFlxPatternStyle;

    /// <summary>
    /// Color for the foreground of the pattern. It is used when the pattern is solid, but not when it is automatic.
    /// </summary>
    FgColorIndex: integer;

    /// <summary>
    /// Color for the background of the pattern.  If the pattern is solid it has no effect, but it is used when pattern is automatic.
    /// </summary>
    BgColorIndex: integer;
  end;

  /// <summary>
  /// Format for one cell.
  /// </summary>
  TFlxFormat=record

    /// <summary>
    /// Cell Font.
    /// </summary>
    Font: TFlxFont;

    /// <summary>
    /// Cell borders.
    /// </summary>
    Borders: TFlxBorders;

    /// <summary>
    /// Format string.  (For example, "yyyy-mm-dd" for a date format, or "#.00" for a numeric 2 decimal format)
    /// <para/>This format string is the same you use in Excel unde "Custom" format when formatting a cell, and it is documented
    /// in Excel documentation. Under <b>"Finding out what format string to use in TFlxFormat.Format"</b> section in <b>UsingFlexCelAPI.pdf</b>
    /// you can find more detailed information on how to create this string.
    /// </summary>
    Format: UTF16String;

    /// <summary>
    /// Fill pattern.
    /// </summary>
    FillPattern: TFlxFillPattern;

    /// <summary>
    /// Horizontal alignment on the cell.
    /// </summary>
    HAlignment: THFlxAlignment;

    /// <summary>
    /// Vertical alignment on the cell.
    /// </summary>
    VAlignment: TVFlxAlignment;

    /// <summary>
    /// Cell is locked.
    /// </summary>
    Locked: boolean;

    /// <summary>
    /// Cell is Hidden.
    /// </summary>
    Hidden: boolean;

    /// <summary>
    /// Parent style. Not currently supported by flexcel.
    /// </summary>
    Parent: integer;

    /// <summary>
    /// Cell wrap.
    /// </summary>
    WrapText: boolean;

    /// <summary>
    /// Shrink to fit.
    /// </summary>
    ShrinkToFit: boolean;

    /// <summary>
    /// Text Rotation in degrees. <para/>
    /// 0 - 90 is up, <para/>
    /// 91 - 180 is down, <para/>
    /// 255 is vertical.
    /// </summary>
    Rotation: byte;

    /// <summary>
    /// Indent value. (in characters)
    /// </summary>
    Indent:   byte;
  end;

  /// <summary> Pointer to a TFlxFormat </summary>
  PFlxFormat=^TFlxFormat;


implementation

end.
