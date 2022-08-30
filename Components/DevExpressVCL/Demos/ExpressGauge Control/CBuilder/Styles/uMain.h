//---------------------------------------------------------------------------

#ifndef uMainH
#define uMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxLabel.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxPC.hpp"
#include "cxTrackBar.hpp"
#include "dxBarBuiltInMenu.hpp"
#include "dxGaugeCircularScale.hpp"
#include "dxGaugeControl.hpp"
#include "dxGaugeCustomScale.hpp"
#include "dxGaugeDigitalScale.hpp"
#include "dxGaugeLinearScale.hpp"
#include "dxGaugeQuantitativeScale.hpp"
#include <ExtCtrls.hpp>
#include "dxGalleryControl.hpp"
class TdxGaugeCustomScaleAccess : public TdxGaugeQuantitativeScale
{
public:
	TdxGaugeScaleType GetScaleType;;
};
class TdxGaugeQuantitativeScaleAccess: public TdxGaugeQuantitativeScale
{
public:
	__property Value;
};//---------------------------------------------------------------------------
class TfrmGaugeStyles : public TForm
{
__published:	// IDE-managed Components
	TcxLabel *cxLabel2;
	TcxTrackBar *cxTrackBar1;
	TcxTabControl *tcStyles;
	TdxGaugeControl *dxGaugeControl1;
	TdxGaugeCircularScale *dxGaugeControl1CircularScale1;
	TdxGaugeDigitalScale *dxGaugeControl1DigitalScale1;
	TdxGaugeLinearScale *dxGaugeControl1LinearScale1;
	TdxGaugeCircularHalfScale *dxGaugeControl1CircularHalfScale1;
	TdxGaugeCircularQuarterLeftScale *dxGaugeControl1CircularQuarterLeftScale1;
	TdxGaugeCircularQuarterRightScale *dxGaugeControl1CircularQuarterRightScale1;
	TdxGaugeContainerScale *dxGaugeControl1ContainerScale1;
	TdxGaugeCircularThreeFourthScale *dxGaugeControl1CircularThreeFourthScale1;
	TTimer *Timer1;
	TcxLookAndFeelController *cxLookAndFeelController1;
	void __fastcall cxTrackBar1PropertiesChange(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall tcStylesChange(TObject *Sender);
	void __fastcall Timer1Timer(TObject *Sender);
private:	// User declarations
	void InitTrackBar();
	void PopulateStyleList();
	void UpdateGauges();
	void UpdateTime();
	String GetStyleNameByCaption(const String ACaption);
	String GetStyleCaption(const String AStyleName);
public:		// User declarations
	__fastcall TfrmGaugeStyles(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmGaugeStyles *frmGaugeStyles;
//---------------------------------------------------------------------------
#endif
