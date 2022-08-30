{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxRichEdit.Export.Rtf.Keywords;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Generics.Defaults, Generics.Collections,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.FloatingObjectFormatting;

type

  TdxRtfExportSR = class
  public const
    OpenGroup = '{';
    CloseGroup = '}';
    RtfSignature = '\rtf1';

    DefaultFontIndex = '\deff';
    StyleTable = '\stylesheet';
    ColorTable = '\colortbl';
    FontTable = '\fonttbl';
    FontCharset = '\fcharset';
    UserTable = '\*\protusertbl';

    DocumentInformation = '\info';
    Password = '\*\password';
    PasswordHash = '\*\passwordhash';
    EnforceProtection = '\enforceprot';
    AnnotationProtection = '\annotprot';
    ReadOnlyProtection = '\readprot';
    ProtectionLevel = '\protlevel';
    NoUICompatible = '\nouicompat';
    DefaultTabWidth = '\deftab';
    HyphenateDocument = '\hyphauto';
    PageFacing = '\facingp';
    DisplayBackgroundShape = '\viewbksp';

    ColorRed = '\red';
    ColorGreen = '\green';
    ColorBlue = '\blue';

    ResetParagraphProperties = '\pard';
    FrameHorizontalPosition = '\posx';
    FrameVerticalPosition = '\posy';
    FrameWidth = '\absw';
    FrameHeight = '\absh';

    FrameNoWrap = '\nowrap';
    FrameWrapOverlay = '\overlay';
    FrameWrapDefault = '\wrapdefault';
    FrameWrapAround = '\wraparound';
    FrameWrapTight = '\wraptight';
    FrameWrapThrough = '\wrapthrough';

    TopParagraphBorder = '\brdrt';
    BottomParagraphBorder = '\brdrb';
    LeftParagraphBorder = '\brdrl';
    RightParagraphBorder = '\brdrr';

    ParagraphHorizontalPositionTypeMargin = '\phmrg';
    ParagraphHorizontalPositionTypePage = '\phpg';
    ParagraphHorizontalPositionTypeColumn = '\phcol';
    ParagraphVerticalPositionTypeMargin = '\pvmrg';
    ParagraphVerticalPositionTypePage = '\pvpg';
    ParagraphVerticalPositionTypeLine = '\pvpara';
    ResetCharacterFormatting = '\plain';
    EndOfParagraph = '\par';
    LeftAlignment = '\ql';
    RightAlignment = '\qr';
    CenterAlignment = '\qc';
    JustifyAlignment = '\qj';
    FirstLineIndentInTwips = '\fi';
    LeftIndentInTwips = '\li';
    LeftIndentInTwips_Lin = '\lin';
    RightIndentInTwips = '\ri';
    RightIndentInTwips_Rin = '\rin';
    AutomaticParagraphHyphenation = '\hyphpar';
    SuppressLineNumbering = '\noline';
    ContextualSpacing = '\contextualspace';
    PageBreakBefore = '\pagebb';
    BeforeAutoSpacing = '\sbauto';
    AfterAutoSpacing = '\saauto';
    KeepWithNext = '\keepn';
    KeepLinesTogether = '\keep';
    WidowOrphanControlOn = '\widctlpar';
    WidowOrphanControlOff = '\nowidctlpar';
    OutlineLevel = '\outlinelevel';
    ParagraphBackgroundColor = '\cbpat';
    RtfLineSpacingValue = '\sl';
    RtfLineSpacingMultiple = '\slmult';
    SpaceBefore = '\sb';
    SpaceAfter = '\sa';
    ListIndex = '\ls';
    LevelIndex = '\ilvl';
    AlternativeText = '\listtext';
    ParagraphNumerationText = '\*\pntext';

    CenteredTab = '\tqc';
    DecimalTab = '\tqdec';
    FlushRightTab = '\tqr';
    TabLeaderDots = '\tldot';
    TabLeaderEqualSign = '\tleq';
    TabLeaderHyphens = '\tlhyph';
    TabLeaderMiddleDots = '\tlmdot';
    TabLeaderThickLine = '\tlth';
    TabLeaderUnderline = '\tlul';
    TabPosition = '\tx';

    AllCapitals = '\caps';
    LangInfo = '\lang';
    LangInfo1 = '\langfe';
    LangInfo2 = '\langnp';
    NoProof = '\noproof';
    HiddenText = '\v';
    FontBold = '\b';
    FontItalic = '\i';
    FontStrikeout = '\strike';
    FontDoubleStrikeout = '\striked1';
    FontUnderline = '\ul';
    FontUnderlineDotted = '\uld';
    FontUnderlineDashed = '\uldash';
    FontUnderlineDashDotted = '\uldashd';
    FontUnderlineDashDotDotted = '\uldashdd';
    FontUnderlineDouble = '\uldb';
    FontUnderlineHeavyWave = '\ulhwave';
    FontUnderlineLongDashed = '\ulldash';
    FontUnderlineThickSingle = '\ulth';
    FontUnderlineThickDotted = '\ulthd';
    FontUnderlineThickDashed = '\ulthdash';
    FontUnderlineThickDashDotted = '\ulthdashd';
    FontUnderlineThickDashDotDotted = '\ulthdashdd';
    FontUnderlineThickLongDashed = '\ulthldash';
    FontUnderlineDoubleWave = '\ululdbwave';
    FontUnderlineWave = '\ulwave';
    FontUnderlineWordsOnly = '\ulw';
    FontNumber = '\f';
    FontSize = '\fs';
    RunBackgroundColor = '\chcbpat';
    RunBackgroundColor2 = '\highlight';
    RunForegroundColor = '\cf';
    RunUnderlineColor = '\ulc';
    RunSuperScript = '\super';
    RunSubScript = '\sub';

    Picture = '\pict';

    PictureWidth = '\picw';
    PictureHeight = '\pich';
    PictureDesiredWidth = '\picwgoal';
    PictureDesiredHeight = '\pichgoal';
    PictureScaleX = '\picscalex';
    PictureScaleY = '\picscaley';
    ShapePicture = '\*\shppict';
    NonShapePicture = '\nonshppict';
    DxImageUri = '\*\dximageuri';

    Space = ' ';
    CLRF = #13#10;

    ResetSectionProperties = '\sectd';
    SectionEndMark = '\sect';
    SectionMarginsLeft = '\marglsxn';
    SectionMarginsRight = '\margrsxn';
    SectionMarginsTop = '\margtsxn';
    SectionMarginsBottom = '\margbsxn';
    SectionMarginsHeaderOffset = '\headery';
    SectionMarginsFooterOffset = '\footery';
    SectionMarginsGutter = '\guttersxn';
    SectionFirstPageHeader = '\headerf';
    SectionOddPageHeader = '\headerr';
    SectionEvenPageHeader = '\headerl';
    SectionFirstPageFooter = '\footerf';
    SectionOddPageFooter = '\footerr';
    SectionEvenPageFooter = '\footerl';

    SectionPageWidth = '\pgwsxn';
    SectionPageHeight = '\pghsxn';
    SectionPageLandscape = '\lndscpsxn';
    PaperKind = '\psz';
    SectionFirstPagePaperSource = '\binfsxn';
    SectionOtherPagePaperSource = '\binsxn';
    SectionOnlyAllowEditingOfFormFields = '\sectunlocked';
    SectionTextFlow = '\stextflow';
    SectionTitlePage = '\titlepg';

    VerticalAlignmentBottom = '\vertal\vertalb';
    VerticalAlignmentTop = '\vertalt';
    VerticalAlignmentCenter = '\vertalc';
    VerticalAlignmentJustify = '\vertalj';

    SectionBreakTypeNextPage = '\sbkpage';
    SectionBreakTypeOddPage = '\sbkodd';
    SectionBreakTypeEvenPage = '\sbkeven';
    SectionBreakTypeColumn = '\sbkcol';
    SectionBreakTypeContinuous = '\sbknone';

    SectionChapterSeparatorHyphen = '\pgnhnsh';
    SectionChapterSeparatorPeriod = '\pgnhnsp';
    SectionChapterSeparatorColon = '\pgnhnsc';
    SectionChapterSeparatorEmDash = '\pgnhnsm';
    SectionChapterSeparatorEnDash = '\pgnhnsn';

    SectionChapterHeaderStyle = '\pgnhn';
    SectionPageNumberingStart = '\pgnstarts';
    SectionPageNumberingContinuous = '\pgncont';
    SectionPageNumberingRestart = '\pgnrestart';

    SectionPageNumberingDecimal = '\pgndec';
    SectionPageNumberingUpperRoman = '\pgnucrm';
    SectionPageNumberingLowerRoman = '\pgnlcrm';
    SectionPageNumberingUpperLetter = '\pgnucltr';
    SectionPageNumberingLowerLetter = '\pgnlcltr';
    SectionPageNumberingArabicAbjad = '\pgnbidia';
    SectionPageNumberingArabicAlpha = '\pgnbidib';
    SectionPageNumberingChosung = '\pgnchosung';
    SectionPageNumberingDecimalEnclosedCircle = '\pgncnum';
    SectionPageNumberingDecimalFullWidth = '\pgndecd';
    SectionPageNumberingGanada = '\pgnganada';
    SectionPageNumberingHindiVowels = '\pgnhindia';
    SectionPageNumberingHindiConsonants = '\pgnhindib';
    SectionPageNumberingHindiNumbers = '\pgnhindic';
    SectionPageNumberingHindiDescriptive = '\pgnhindid';
    SectionPageNumberingThaiLetters = '\pgnthaia';
    SectionPageNumberingThaiNumbers = '\pgnthaib';
    SectionPageNumberingThaiDescriptive = '\pgnthaic';
    SectionPageNumberingVietnameseDescriptive = '\pgnvieta';

    SectionLineNumberingContinuous = '\linecont';
    SectionLineNumberingStartingLineNumber = '\linestarts';
    SectionLineNumberingRestartNewPage = '\lineppage';
    SectionLineNumberingRestartNewSection = '\linerestart';
    SectionLineNumberingStep = '\linemod';
    SectionLineNumberingDistance = '\linex';

    SectionColumnsCount = '\cols';
    SectionSpaceBetweenColumns = '\colsx';
    SectionColumnsDrawVerticalSeparator = '\linebetcol';

    SectionColumnNumber = '\colno';
    SectionColumnWidth = '\colw';
    SectionColumnSpace = '\colsr';

    SectionFootNotePlacementBelowText = '\sftntj';
    SectionFootNotePlacementPageBottom = '\sftnbj';
    SectionFootNoteNumberingStart = '\sftnstart';
    SectionFootNoteNumberingRestartEachPage = '\sftnrstpg';
    SectionFootNoteNumberingRestartEachSection = '\sftnrestart';
    SectionFootNoteNumberingRestartContinuous = '\sftnrstcont';

    SectionFootNoteNumberingFormatDecimal = '\sftnnar';
    SectionFootNoteNumberingFormatUpperRoman = '\sftnnruc';
    SectionFootNoteNumberingFormatLowerRoman = '\sftnnrlc';
    SectionFootNoteNumberingFormatUpperLetter = '\sftnnauc';
    SectionFootNoteNumberingFormatLowerLetter = '\sftnnalc';
    SectionFootNoteNumberingFormatChicago = '\sftnnchi';
    SectionFootNoteNumberingFormatChosung = '\sftnnchosung';
    SectionFootNoteNumberingFormatDecimalEnclosedCircle = '\sftnncnum';
    SectionFootNoteNumberingFormatDecimalFullWidth = '\sftnndbar';
    SectionFootNoteNumberingFormatGanada = '\sftnnganada';

    SectionEndNoteNumberingStart = '\saftnstart';
    SectionEndNoteNumberingRestartEachSection = '\saftnrestart';
    SectionEndNoteNumberingRestartContinuous = '\saftnrstcont';

    SectionEndNoteNumberingFormatDecimal = '\saftnnar';
    SectionEndNoteNumberingFormatUpperRoman = '\saftnnruc';
    SectionEndNoteNumberingFormatLowerRoman = '\saftnnrlc';
    SectionEndNoteNumberingFormatUpperLetter = '\saftnnauc';
    SectionEndNoteNumberingFormatLowerLetter = '\saftnnalc';
    SectionEndNoteNumberingFormatChicago = '\saftnnchi';
    SectionEndNoteNumberingFormatChosung = '\saftnnchosung';
    SectionEndNoteNumberingFormatDecimalEnclosedCircle = '\saftnncnum';
    SectionEndNoteNumberingFormatDecimalFullWidth = '\saftnndbar';
    SectionEndNoteNumberingFormatGanada = '\saftnnganada';

    LegacyPaperWidth = '\paperw';
    LegacyPaperHeight = '\paperh';
    LegacyLandscape = '\landscape';
    LegacyPageNumberingStart = '\pgnstart';
    LegacyMarginsLeft = '\margl';
    LegacyMarginsRight = '\margr';
    LegacyMarginsTop = '\margt';
    LegacyMarginsBottom = '\margb';
    LegacyMarginsGutter = '\gutter';
    LegacyMarginsGutterAtRight = '\rtlgutter';

    FootNotePlacementBelowText = '\ftntj';
    FootNotePlacementPageBottom = '\ftnbj';
    FootNoteNumberingStart = '\ftnstart';
    FootNoteNumberingRestartEachPage = '\ftnrstpg';
    FootNoteNumberingRestartEachSection = '\ftnrestart';
    FootNoteNumberingRestartContinuous = '\ftnrstcont';
    FootNoteNumberingFormatDecimal = '\ftnnar';
    FootNoteNumberingFormatUpperRoman = '\ftnnruc';
    FootNoteNumberingFormatLowerRoman = '\ftnnrlc';
    FootNoteNumberingFormatUpperLetter = '\ftnnauc';
    FootNoteNumberingFormatLowerLetter = '\ftnnalc';
    FootNoteNumberingFormatChicago = '\ftnnchi';
    FootNoteNumberingFormatChosung = '\ftnnchosung';
    FootNoteNumberingFormatDecimalEnclosedCircle = '\ftnncnum';
    FootNoteNumberingFormatDecimalFullWidth = '\ftnndbar';
    FootNoteNumberingFormatGanada = '\ftnnganada';

    EndNotePlacementEndOfSection = '\aendnotes';
    EndNotePlacementEndOfDocument = '\aenddoc';
    EndNoteNumberingStart = '\aftnstart';
    EndNoteNumberingRestartEachSection = '\aftnrestart';
    EndNoteNumberingRestartContinuous = '\aftnrstcont';
    EndNoteNumberingFormatDecimal = '\aftnnar';
    EndNoteNumberingFormatUpperRoman = '\aftnnruc';
    EndNoteNumberingFormatLowerRoman = '\aftnnrlc';
    EndNoteNumberingFormatUpperLetter = '\aftnnauc';
    EndNoteNumberingFormatLowerLetter = '\aftnnalc';
    EndNoteNumberingFormatChicago = '\aftnnchi';
    EndNoteNumberingFormatChosung = '\aftnnchosung';
    EndNoteNumberingFormatDecimalEnclosedCircle = '\aftnncnum';
    EndNoteNumberingFormatDecimalFullWidth = '\aftnndbar';
    EndNoteNumberingFormatGanada = '\aftnnganada';

    Field = '\field';
    FieldInstructions = '\*\fldinst';
    FieldResult = '\fldrslt';
    FieldCodeView = '\dxfldcodeview';

    FieldMapData = '\*\mmodsofldmpdata';
    FieldTypeNull = '\mmfttypenull';
    FieldTypeColumn = '\mmfttypedbcolumn';
    FieldTypeAddress = '\mmfttypeaddress';
    FieldTypeSalutation = '\mmfttypesalutation';
    FieldTypeMapped = '\mmfttypemapped';
    FieldTypeBarcode = '\mmfttypebarcode';
    MailMergeDataSourceObjectName = '\mmodsoname';
    MailMergeDataSourceObjectMappedName = '\mmodsomappedname';
    MailMergeDataSourceObjectColumnIndex = '\mmodsofmcolumn';
    MailMergeDataSourceObjectDynamicAddress = '\mmodsodynaddr';
    MailMergeDataSourceObjectLanguageId = '\mmodsolid';

    BookmarkStart = '\*\bkmkstart';
    BookmarkEnd = '\*\bkmkend';
    RangePermissionStart = '\*\protstart';
    RangePermissionEnd = '\*\protend';
    CommentStart = '\*\atrfstart';
    CommentEnd = '\*\atrfend';
    CommentId = '\*\atnid';
    CommentAuthor = '\*\atnauthor';
    CommentTime = '\*\atntime';
    CommentChatn = '\chatn';
    CommentAnnotation = '\*\annotation';
    CommentDate = '\*\atndate';
    CommentRef = '\*\atnref';
    CommentParent = '\*\atnparent';

    HyperlinkFieldType = 'HYPERLINK';

    DocumentVariable = '\*\docvar';
    FootNote = '\footnote';
    FootNoteReference = '\chftn';
    EndNote = '\ftnalt';

    PageBackground = '\*\background';
    Shape = '\shp';
    ShapeInstance = '\*\shpinst';
    ShapeText = '\shptxt';
    ShapeLeft = '\shpleft';
    ShapeRight = '\shpright';
    ShapeTop = '\shptop';
    ShapeBottom = '\shpbottom';
    ShapeZOrder = '\shpz';
    ShapeLegacyHorizontalPositionTypePage = '\shpbxpage';
    ShapeLegacyHorizontalPositionTypeMargin = '\shpbxmargin';
    ShapeLegacyHorizontalPositionTypeColumn = '\shpbxcolumn';
    ShapeIgnoreLegacyHorizontalPositionType = '\shpbxignore';
    ShapeLegacyVerticalPositionTypePage = '\shpbypage';
    ShapeLegacyVerticalPositionTypeMargin = '\shpbymargin';
    ShapeLegacyVerticalPositionTypeParagraph = '\shpbypara';
    ShapeIgnoreLegacyVerticalPositionType = '\shpbyignore';
    ShapeWrapTextType = '\shpwr';
    ShapeWrapTextTypeZOrder = '\shpfblwtxt';
    ShapeWrapTextSide = '\shpwrk';
    ShapeLocked = '\shplockanchor';
    ShapeProperty = '\sp';
    ShapePropertyName = '\sn';
    ShapePropertyValue = '\sv';
    ShapeResult = '\shprslt';
    ShapeDoNotLay = '\splytwnine';
    HtmlAutoSpacing = '\htmautsp';
    ParagraphGroupPropertiesTable = '\*\pgptbl';
    ParagraphGroupProperties = '\pgp';
    ParagraphGroupPropertiesId = '\ipgp';

  {$region Tables}
    ResetTableProperties = '\trowd';
    InTableParagraph = '\intbl';
    TableEndCell = '\cell';
    NestedTableEndCell = '\nestcell';
    TableEndRow = '\row';
    NestedTableEndRow = '\nestrow';
    NestedTableProperties = '\*\nesttableprops';
    NoNestedTable = '\nonesttables';
    ParagraphNestingLevel = '\itap';
    TableCellRight = '\cellx';
    TableCellPreferredWidth = '\clwWidth';
    TableCellPreferredWidthType = '\clftsWidth';
    TableCellBottomMargin = '\clpadb';
    TableCellLeftMargin = '\clpadl';
    TableCellRightMargin = '\clpadr';
    TableCellTopMargin = '\clpadt';
    TableCellBottomMarginType = '\clpadfb';
    TableCellLeftMarginType = '\clpadfl';
    TableCellRightMarginType = '\clpadfr';
    TableCellTopMarginType = '\clpadft';
    TableRowIndex = '\irow';
    TableRowBandIndex = '\irowband';
    TableRowLeftAlignment = '\trql';
    TableRowRightAlignment = '\trqr';
    TableRowCenterAlignment = '\trqc';
    TableIndent = '\tblind';
    TableIndentType = '\tblindtype';

    TableCellBottomBorder = '\clbrdrb';
    TableCellTopBorder = '\clbrdrt';
    TableCellLeftBorder = '\clbrdrl';
    TableCellRightBorder = '\clbrdrr';
    TableCellUpperLeftToLowerRightBorder = '\cldglu';
    TableCellUpperRightToLowerLeftBorder = '\cldgll';
    TableCellStartHorizontalMerging = '\clmgf';
    TableCellContinueHorizontalMerging = '\clmrg';
    TableCellStartVerticalMerging = '\clvmgf';
    TableCellContinueVerticalMerging = '\clvmrg';
    TableCellTextTopAlignment = '\clvertalt';
    TableCellTextCenterAlignment = '\clvertalc';
    TableCellTextBottomAlignment = '\clvertalb';
    TableCellLeftToRightTopToBottomTextDirection = '\cltxlrtb';
    TableCellTopToBottomRightToLeftTextDirection = '\cltxtbrl';
    TableCellBottomToTopLeftToRightTextDirection = '\cltxbtlr';
    TableCellLeftToRightTopToBottomVerticalTextDirection = '\cltxlrtbv';
    TableCellTopToBottomRightToLeftVerticalTextDirection = '\cltxtbrlv';
    TableCellFitText = '\clFitText';
    TableCellNoWrap = '\clNoWrap';
    TableCellHideMark = '\clhidemark';
    TableCellBackgroundColor = '\clcbpat';
    TableCellForegroundColor = '\clcfpat';
    TableCellShading = '\clshdng';
    TableTopBorder = '\trbrdrt';
    TableLeftBorder = '\trbrdrl';
    TableBottomBorder = '\trbrdrb';
    TableRightBorder = '\trbrdrr';
    TableHorizontalBorder = '\trbrdrh';
    TableVerticalBorder = '\trbrdrv';
    TableRowHorizontalAnchorColumn = '\tphcol';
    TableRowHorizontalAnchorMargin = '\tphmrg';
    TableRowHorizontalAnchorPage = '\tphpg';
    TableRowVerticalAnchorMargin = '\tpvmrg';
    TableRowVerticalAnchorParagraph = '\tpvpara';
    TableRowVerticalAnchorPage = '\tpvpg';
    TableRowHorizontalAlignCenter = '\tposxc';
    TableRowHorizontalAlignInside = '\tposxi';
    TableRowHorizontalAlignLeft = '\tposxl';
    TableRowHorizontalAlignOutside = '\tposxo';
    TableRowHorizontalAlignRight = '\tposxr';
    TableRowHorizontalPosition = '\tposx';
    TableRowHorizontalPositionNeg = '\tposnegx';
    TableRowVerticalAlignBottom = '\tposyb';
    TableRowVerticalAlignCenter = '\tposyc';
    TableRowVerticalAlignInline = '\tposyil';
    TableRowVerticalAlignInside = '\tposyin';
    TableRowVerticalAlignOutside = '\tposyout';
    TableRowVerticalAlignTop = '\tposyt';
    TableRowVerticalPosition = '\tposy';
    TableRowVerticalPositionNeg = '\tposnegy';
    TableRowLeftFromText = '\tdfrmtxtLeft';
    TableRowBottomFromText = '\tdfrmtxtBottom';
    TableRowRightFromText = '\tdfrmtxtRight';
    TableRowTopFromText = '\tdfrmtxtTop';
    TableNoOverlap = '\tabsnoovrlp';
    TableHalfSpaceBetweenCells = '\trgaph';
    TableRowLeft = '\trleft';
    TableRowHeight = '\trrh';
    TableRowHeader = '\trhdr';
    TableRowCantSplit = '\trkeep';
    TablePreferredWidth = '\trwWidth';
    TablePreferredWidthType = '\trftsWidth';
    TableRowWidthBefore = '\trwWidthB';
    TableRowWidthBeforeType = '\trftsWidthB';
    TableRowWidthAfter = '\trwWidthA';
    TableRowWidthAfterType = '\trftsWidthA';
    TableLayout = '\trautofit';
    TableCellSpacingBottom = '\trspdb';
    TableCellSpacingLeft = '\trspdl';
    TableCellSpacingRight = '\trspdr';
    TableCellSpacingTop = '\trspdt';
    TableCellSpacingBottomType = '\trspdfb';
    TableCellSpacingLeftType = '\trspdfl';
    TableCellSpacingRightType = '\trspdfr';
    TableCellSpacingTopType = '\trspdft';
    TableCellMarginsBottom = '\trpaddb';
    TableCellMarginsLeft = '\trpaddl';
    TableCellMarginsRight = '\trpaddr';
    TableCellMarginsTop = '\trpaddt';
    TableCellMarginsBottomType = '\trpaddfb';
    TableCellMarginsLeftType = '\trpaddfl';
    TableCellMarginsRightType = '\trpaddfr';
    TableCellMarginsTopType = '\trpaddft';
    TableApplyFirstRow = '\tbllkhdrrows';
    TableApplyLastRow = '\tbllklastrow';
    TableApplyFirstColumn = '\tbllkhdrcols';
    TableApplyLastColumn = '\tbllklastcol';
    TableDoNotApplyRowBanding = '\tbllknorowband';
    TableDoNotApplyColumnBanding = '\tbllknocolband';
    TableLastRow = '\lastrow';
  {$endregion}

  {$region Table Style}
    TableStyleResetTableProperties = '\tsrowd';
    TableStyleCellVerticalAlignmentTop = '\tsvertalt';
    TableStyleCellVerticalAlignmentCenter = '\tsvertalc';
    TableStyleCellVerticalAlignmentBottom = '\tsvertalb';

    TableStyleRowBandSize = '\tscbandsh';
    TableStyleColumnBandSize = '\tscbandsv';
    TableStyleCellBackgroundColor = '\tscellcbpat';

    TableStyleTopCellBorder = '\tsbrdrt';
    TableStyleLeftCellBorder = '\tsbrdrl';
    TableStyleBottomCellBorder = '\tsbrdrb';
    TableStyleRightCellBorder = '\tsbrdrr';
    TableStyleHorizontalCellBorder = '\tsbrdrh';
    TableStyleVerticalCellBorder = '\tsbrdrv';

    TableStyleCellNoWrap = '\tsnowrap';
    TableStyleTableBottomCellMargin = '\tscellpaddb';
    TableStyleTableLeftCellMargin = '\tscellpaddl';
    TableStyleTableRightCellMargin = '\tscellpaddr';
    TableStyleTableTopCellMargin = '\tscellpaddt';
    TableStyleTableBottomCellMarginUnitType = '\tscellpaddfb';
    TableStyleTableLeftCellMarginUnitType = '\tscellpaddfl';
    TableStyleTableRightCellMarginUnitType = '\tscellpaddfr';
    TableStyleTableTopCellMarginUnitType = '\tscellpaddft';
    TableStyleUpperLeftToLowerRightBorder = '\tsbrdrdgl';
    TableStyleUpperRightToLowerLeftBorder = '\tsbrdrdgr';
  {$endregion}
  {$region Table Conditional Style}
    TableConditionalStyleFirstRow = '\tscfirstrow';
    TableConditionalStyleLastRow = '\tsclastrow';
    TableConditionalStyleFirstColumn = '\tscfirstcol';
    TableConditionalStyleLastColumn = '\tsclastcol';
    TableConditionalStyleOddRowBanding = '\tscbandhorzodd';
    TableConditionalStyleEvenRowBanding = '\tscbandhorzeven';
    TableConditionalStyleOddColumnBanding = '\tscbandvertodd';
    TableConditionalStyleEvenColumnBanding = '\tscbandverteven';
    TableConditionalStyleTopLeftCell = '\tscnwcell';
    TableConditionalStyleTopRightCell = '\tscnecell';
    TableConditionalStyleBottomLeftCell = '\tscswcell';
    TableConditionalStyleBottomRightCell = '\tscsecell';
  {$endregion}
  {$region Border}
    NoTableBorder = '\brdrtbl';
    NoBorder = '\brdrnil';
    BorderWidth = '\brdrw';
    BorderColor = '\brdrcf';
    BorderFrame = '\brdrframe';
    BorderSpace = '\brsp';
    BorderArtIndex = '\brdrart';
    BorderSingleWidth = '\brdrs';
    BorderDoubleWidth = '\brdrth';
    BorderShadow = '\brdrsh';
    BorderDouble = '\brdrdb';
    BorderDotted = '\brdrdot';
    BorderDashed = '\brdrdash';
    BorderSingle = '\brdrhair';
    BorderDashedSmall = '\brdrdashsm';
    BorderDotDashed = '\brdrdashd';
    BorderDotDotDashed = '\brdrdashdd';
    BorderInset = '\brdrinset';
    BorderNone = '\brdrnone';
    BorderOutset = '\brdroutset';
    BorderTriple = '\brdrtriple';
    BorderThickThinSmall = '\brdrtnthsg';
    BorderThinThickSmall = '\brdrthtnsg';
    BorderThinThickThinSmall = '\brdrtnthtnsg';
    BorderThickThinMedium = '\brdrtnthmg';
    BorderThinThickMedium = '\brdrthtnmg';
    BorderThinThickThinMedium = '\brdrtnthtnmg';
    BorderThickThinLarge = '\brdrtnthlg';
    BorderThinThickLarge = '\brdrthtnlg';
    BorderThinThickThinLarge = '\brdrtnthtnlg';
    BorderWavy = '\brdrwavy';
    BorderDoubleWavy = '\brdrwavydb';
    BorderDashDotStroked = '\brdrdashdotstr';
    BorderThreeDEmboss = '\brdremboss';
    BorderThreeDEngrave = '\brdrengrave';
  {$endregion}
  {$region NumberingList}
    NumberingListTable = '\*\listtable';
    ListOverrideTable = '\*\listoverridetable';

    NumberingList = '\list';
    NumberingListId = '\listid';
    NumberingListTemplateId = '\listtemplateid';
    NumberingListStyleId = '\liststyleid';
    NumberingListStyleName = '\*\liststylename ';

    NumberingListName = '\listname ;';
    NumberingListHybrid = '\listhybrid';
    ListLevel = '\listlevel';

    ListLevelStart = '\levelstartat';
    ListLevelTentative = '\lvltentative';
    ListLevelNumberingFormat = '\levelnfc';
    ListLevelAlignment = '\leveljc';
    ListLevelNumberingFormatN = '\levelnfcn';
    ListLevelAlignmentN = '\leveljcn';

    LisLeveltOld = '\levelold';
    ListLevelPrev = '\levelprev';
    ListLevelPrevSpase = '\levelprevspace';
    ListLevelSpace = '\levelspace';
    ListLevelIntdent = '\levelindent';
    ListLevelNumbers = '\levelnumbers';
    ListLevelText = '\leveltext';
    LevelTemplateId = '\leveltemplateid';
    ListLevelFollow = '\levelfollow';
    ListLevelLegal = '\levellegal';
    ListLevelNoRestart = '\levelnorestart';
    ListLevelPicture = '\levelpicture';
    ListLevelPictureNoSize = '\levelpicturenosize';
    ListLevelLegacy = '\levelold';
    ListLevelLegacySpace = '\levelspace';
    ListLevelLegacyIndent = '\levelindent';

    ListOverride = '\listoverride';
    ListOverrideListId = '\listid';
    ListOverrideCount = '\listoverridecount';
    ListOverrideLevel = '\lfolevel';

    ListOverrideFormat = '\listoverrideformat';
    ListOverrideStart = '\listoverridestartat';
    ListOverrideStartValue = '\levelstartat';
    ListOverrideListLevel = '\listlevel';
  {$endregion}
  {$region DefaultProperties}
    DefaultCharacterProperties = '\*\defchp';
    DefaultParagraphProperties = '\*\defpap';
  {$endregion}
  {$region Style}
    StyleSheet = '\stylesheet';
    ParagraphStyle = '\s';
    CharacterStyle = '\*\cs';
    CharacterStyleIndex = '\cs';
    TableStyle = '\*\ts';
    TableStyleIndex = '\ts';
    TableStyleCellIndex = '\yts';
    ParentStyle = '\sbasedon';
    LinkedStyle = '\slink';
    NextStyle = '\snext';
    QuickFormatStyle = '\sqformat';
  {$endregion}
  public const
{$REGION 'FloatingObjectTextWrapTypeTable'}
      FloatingObjectTextWrapTypeTable: array[TdxFloatingObjectTextWrapType] of Integer = (3, 1, 4, 5, 2, 3);
{$ENDREGION}
{$REGION 'FloatingObjectTextWrapSideTable'}
      FloatingObjectTextWrapSideTable: array[TdxFloatingObjectTextWrapSide] of Integer = (0, 1, 2, 3);
{$ENDREGION}
{$REGION 'FloatingObjectHorizontalPositionTypeTable'}
      FloatingObjectHorizontalPositionTypeTable: array[TdxFloatingObjectHorizontalPositionType] of integer = (1, 3, 2, 0, 4, 5, 6, 7);
{$ENDREGION}
{$REGION 'FloatingObjectHorizontalPositionAlignmentTable'}
      FloatingObjectHorizontalPositionAlignmentTable: array[TdxFloatingObjectHorizontalPositionAlignment] of Integer = (0, 1, 2, 3, 4, 5);
{$ENDREGION}
{$REGION 'FloatingObjectVerticalPositionTypeTable'}
      FloatingObjectVerticalPositionTypeTable: array[TdxFloatingObjectVerticalPositionType] of Integer = (1, 3, 2, 0, 4, 5, 6, 7);
{$ENDREGION}
{$REGION 'FloatingObjectVerticalPositionAlignmentTable'}
      FloatingObjectVerticalPositionAlignmentTable: array[TdxFloatingObjectVerticalPositionAlignment] of Integer = (0, 1, 2, 3, 4, 5);
{$ENDREGION}
{$REGION 'FloatingObjectRelativeFromHorizontalTable'}
      FloatingObjectRelativeFromHorizontalTable: array[TdxFloatingObjectRelativeFromHorizontal] of Integer = (0, 1, 2, 3, 4, 5);
{$ENDREGION}
{$REGION 'FloatingObjectRelativeFromVerticalTable'}
      FloatingObjectRelativeFromVerticalTable: array[TdxFloatingObjectRelativeFromVertical] of Integer = (0, 1, 2, 3, 4, 5);
{$ENDREGION}
  end;

implementation

end.

