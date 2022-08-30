//---------------------------------------------------------------------------

#ifndef RibbonNotepadDemoOptionsH
#define RibbonNotepadDemoOptionsH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include <dxRibbonSkins.hpp>
#include <cxGraphics.hpp>
#include <cxControls.hpp>
#include <cxLookAndFeels.hpp>
#include <cxLookAndFeelPainters.hpp>
#include <cxContainer.hpp>
#include <cxEdit.hpp>
#include <cxLabel.hpp>
#include <Menus.hpp>
#include <cxButtons.hpp>
#include <dxBevel.hpp>
#include <cxTextEdit.hpp>
#include <cxMaskEdit.hpp>
#include <cxDropDownEdit.hpp>
//---------------------------------------------------------------------------

enum TRibbonDemoStyle {rdsOffice2007, rdsOffice2010, rdsOffice2013, rdsOffice2016, rdsOffice2016Tablet, rdsOffice2019, rdsScenic};

struct TScreenTipOptions
{
  bool ShowScreenTips;
  bool ShowDescripitons;
};

class TRibbonDemoOptionsForm : public TForm
{
__published:	// IDE-managed Components
	TcxButton *btnOk;
    TcxButton *btnCancel;
    TcxComboBox *cbColorScheme;
    TcxComboBox *cbColorSchemeAccent;
    TcxComboBox *cbRibbonStyle;
    TcxComboBox *cbScreenTipStyle;
    TdxBevel *gbPanel;
    TcxLabel *lblColorScheme;
    TcxLabel *lblColorSchemeAccent;
    TcxLabel *lblRibbonStyle;
    TcxLabel *lblScreenTipStyle;
	
	void __fastcall cbRibbonStyleSelect(TObject *Sender);
public:		// User declarations
	__fastcall TRibbonDemoOptionsForm(TComponent* Owner);
	void LoadOptions(String AColorSchemeName, TScreenTipOptions AScreenTipOptions,
		TRibbonDemoStyle AStyle, TdxRibbonColorSchemeAccent AColorSchemeAccent);
	void SaveOptions(String *AColorSchemeName, TScreenTipOptions *AScreenTipOptions,
		TRibbonDemoStyle *AStyle, TdxRibbonColorSchemeAccent *AColorSchemeAccent);
};
void PopulateColorSchemeAccents(TStrings *AItems);
void PopulateColorSchemes(TStrings *AItems, TdxRibbonStyle ARibbonStyle);
void PopulateRibbonStyles(TStrings *AItems);
//---------------------------------------------------------------------------
extern PACKAGE TRibbonDemoOptionsForm *RibbonDemoOptionsForm;
//---------------------------------------------------------------------------
BOOL ExecuteRibbonDemoOptions(String *AColorSchemeName,
	TScreenTipOptions *AScreenTipOptions, TRibbonDemoStyle *AStyle,
	TdxRibbonColorSchemeAccent *AColorSchemeAccent);
	
TdxRibbonStyle RibbonDemoStyleToRibbonStyle(TRibbonDemoStyle ADemoStyle);

#endif