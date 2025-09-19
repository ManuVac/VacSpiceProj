Public Class CtrlLabel
    Private v_cvPression As String
    Public Property CvPression() As String
        Get
            Return v_cvPression
        End Get
        Set(ByVal value As String)
            Label1.Text = value
            v_cvPression = value
        End Set
    End Property

End Class
