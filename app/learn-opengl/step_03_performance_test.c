
/* from http://openglbook.com/chapter-1-getting-started.html */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <GL/glew.h>
#include <GL/freeglut.h>
#include "vertices.h"
#define WINDOW_TITLE_PREFIX "Chapter 2"

int CurrentWidth = 800,
   CurrentHeight = 600,
   WindowHandle = 0;
GLuint
VertexShaderId,
   FragmentShaderId,
   ProgramId,
   VaoId,
   VboId
      ;

const GLchar* VertexShader =
{
   "#version 330\n"				\
   "layout(location=0) in vec4 inPosition;\n"	\
   "void main(void)\n"							\
   "{\n"								\
   "  gl_Position = vec4(inPosition.x, inPosition.y, 0.0f, 1.0f);\n"		\
   "}\n"
};
/* hardcoding the rendered color to green */
const GLchar* FragmentShader =
{
   "#version 330\n"							\
   "out vec4 outputColor;\n"						\
   "void main(void)\n"							\
   "{\n"								\
   "  outputColor = vec4(0.0f, 1.0f, 0.0f, 1.0f);\n"			\
   "}\n"
};

unsigned FrameCount = 0;

void Initialize(int, char*[]);
void InitWindow(int, char*[]);
void ResizeFunction(int, int);
void RenderFunction(void);
void TimerFunction(int);
void IdleFunction(void);
void Cleanup(void);
void CreateVBO(void);
void DestroyVBO(void);
void CreateShaders(void);
void DestroyShaders(void);

int main(int argc, char* argv[])
{
   int vertex_texture_units;

   Initialize(argc, argv);

   glGetIntegerv(GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS, &vertex_texture_units);

   if(!vertex_texture_units) {
      fprintf(stderr, "Your graphics cards does not support texture lookups in the vertex shader!\n");
      // exit here or use another method to render the graph
   }
   else {fprintf(stdout, "Vertex_texture_units: %i\n", vertex_texture_units);}

   glutMainLoop();

   exit(EXIT_SUCCESS);
}

void Initialize(int argc, char* argv[])
{
   GLenum GlewInitResult;

   InitWindow(argc, argv);

   glewExperimental = GL_TRUE;
   GlewInitResult = glewInit();

   if (GLEW_OK != GlewInitResult) {
      fprintf(
	     stderr,
	     "ERROR: %s\n",
	     glewGetErrorString(GlewInitResult)
		 );
	 exit(EXIT_FAILURE);
      }

      fprintf(
	  stdout,
	  "INFO: OpenGL Version: %s\n",
	  glGetString(GL_VERSION)
	      );

      CreateShaders();
      CreateVBO();
      glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
   }

   void InitWindow(int argc, char* argv[])
   {
      glutInit(&argc, argv);

      /* http://stackoverflow.com/questions/23844813/opengl-program-compiles-but-gives-error-when-run-ubuntu-14-04 */
      /* use 3.3 context as my video card supports only that as reported by glxinfo */
      glutInitContextVersion(3, 3);
      glutInitContextFlags(GLUT_FORWARD_COMPATIBLE);
      glutInitContextProfile(GLUT_CORE_PROFILE);

      glutSetOption(
	  GLUT_ACTION_ON_WINDOW_CLOSE,
	  GLUT_ACTION_GLUTMAINLOOP_RETURNS
		    );

      glutInitWindowSize(CurrentWidth, CurrentHeight);

      glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGBA);

      WindowHandle = glutCreateWindow(WINDOW_TITLE_PREFIX);

      if(WindowHandle < 1) {
	 fprintf(
	     stderr,
	     "ERROR: Could not create a new rendering window.\n"
		 );
	 exit(EXIT_FAILURE);
      }

      glutReshapeFunc(ResizeFunction);
      glutDisplayFunc(RenderFunction);
      glutIdleFunc(IdleFunction);
      glutTimerFunc(0, TimerFunction, 0);
      glutCloseFunc(Cleanup);
   }

void ResizeFunction(int Width, int Height)
{
   CurrentWidth = Width;
   CurrentHeight = Height;
   glViewport(0, 0, CurrentWidth, CurrentHeight);
}

void RenderFunction(void)
{
   float startTime = glutGet(GLUT_ELAPSED_TIME);
   float endTime = startTime;
   CreateShaders();
   CreateVBO();
   glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
   ++FrameCount;
   glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
   /*	 glDrawArrays(GL_TRIANGLE_STRIP, 0, 4); */
   /*    glDrawArrays(GL_TRIANGLES, 0, 4); */
   /*    glDrawArrays(GL_POINTS, 0, 4); */
   /*    glDrawArrays(GL_LINES, 0, 4); */
   glDrawArrays(GL_LINE_LOOP, 0, 10000);

   glutSwapBuffers();
   glutPostRedisplay();
   endTime = glutGet(GLUT_ELAPSED_TIME);
   fprintf(stdout, "RenderFunction: took %.0f milliseconds\n", endTime - startTime);
   Cleanup();
}
void IdleFunction(void)
{
   glutPostRedisplay();
}

void TimerFunction(int Value)
{
   if (0 != Value) {
      char* TempString = (char*)
	       malloc(512 + strlen(WINDOW_TITLE_PREFIX));

	 sprintf(
	     TempString,
	     "%s: %d Frames Per Second @ %d x %d",
	     WINDOW_TITLE_PREFIX,
	     FrameCount * 4,
	     CurrentWidth,
	     CurrentHeight
		 );

	 fprintf(
	     stdout,
	     "%s: %d Frames Per Second @ %d x %d\n",
	     WINDOW_TITLE_PREFIX,
	     FrameCount * 4,
	     CurrentWidth,
	     CurrentHeight
		 );

	 glutSetWindowTitle(TempString);
	 free(TempString);
      }

      FrameCount = 0;
      glutTimerFunc(250, TimerFunction, 1);
   }
   void Cleanup(void)
   {
      DestroyShaders();
      DestroyVBO();
   }
   void CreateVBO(void)
   {
      float startTime = glutGet(GLUT_ELAPSED_TIME);
      float endTime = startTime;

      GLenum ErrorCheckValue = glGetError();

      glGenVertexArrays(1, &VaoId);
      glBindVertexArray(VaoId);

      glGenBuffers(1, &VboId);
      glBindBuffer(GL_ARRAY_BUFFER, VboId);
      glBufferData(GL_ARRAY_BUFFER, sizeof(Vertices), Vertices, GL_STATIC_DRAW);
      glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 0, 0);
      glEnableVertexAttribArray(0);
      /*    unbind the buffer  */
      glBindBuffer(GL_ARRAY_BUFFER, 0);

      ErrorCheckValue = glGetError();
      if (ErrorCheckValue != GL_NO_ERROR)
      {
	 fprintf(
	     stderr,
	     "ERROR: Could not create a VBO: %s \n",
	     gluErrorString(ErrorCheckValue)
		 );

	 exit(-1);
      }
      endTime = glutGet(GLUT_ELAPSED_TIME);
      fprintf(stdout, "CreateVBO: took %.0f milliseconds\n", endTime - startTime);
   }
   void DestroyVBO(void)
   {
      GLenum ErrorCheckValue = glGetError();

      glDisableVertexAttribArray(0);

      glBindBuffer(GL_ARRAY_BUFFER, 0);
      glDeleteBuffers(1, &VboId);

      glBindVertexArray(0);
      glDeleteVertexArrays(1, &VaoId);

      ErrorCheckValue = glGetError();
      if (ErrorCheckValue != GL_NO_ERROR)
      {
	 fprintf(
	     stderr,
	     "ERROR: Could not destroy the VBO: %s \n",
	     gluErrorString(ErrorCheckValue)
		 );

	 exit(-1);
      }
   }
   void CreateShaders(void)
   {
      GLenum ErrorCheckValue = glGetError();

      float startTime = glutGet(GLUT_ELAPSED_TIME);
      float endTime = startTime;
      VertexShaderId = glCreateShader(GL_VERTEX_SHADER);
      glShaderSource(VertexShaderId, 1, &VertexShader, NULL);
      glCompileShader(VertexShaderId);

      FragmentShaderId = glCreateShader(GL_FRAGMENT_SHADER);
      glShaderSource(FragmentShaderId, 1, &FragmentShader, NULL);
      glCompileShader(FragmentShaderId);

      ProgramId = glCreateProgram();
      glAttachShader(ProgramId, VertexShaderId);
      glAttachShader(ProgramId, FragmentShaderId);
      glLinkProgram(ProgramId);
      glUseProgram(ProgramId);

      ErrorCheckValue = glGetError();
      if (ErrorCheckValue != GL_NO_ERROR)
      {
	 fprintf(
	     stderr,
	     "ERROR: Could not create the shaders: %s \n",
	     gluErrorString(ErrorCheckValue)
		 );

	 exit(-1);
      }
      endTime = glutGet(GLUT_ELAPSED_TIME);
      fprintf(stdout, "CreateShaders: took %.0f milliseconds\n", endTime - startTime);
   }
   void DestroyShaders(void)
   {
      GLenum ErrorCheckValue = glGetError();

      glUseProgram(0);

      glDetachShader(ProgramId, VertexShaderId);
      glDetachShader(ProgramId, FragmentShaderId);

      glDeleteShader(FragmentShaderId);
      glDeleteShader(VertexShaderId);

      glDeleteProgram(ProgramId);

      ErrorCheckValue = glGetError();
      if (ErrorCheckValue != GL_NO_ERROR)
      {
	 fprintf(
	     stderr,
	     "ERROR: Could not destroy the shaders: %s \n",
	     gluErrorString(ErrorCheckValue)
		 );

	 exit(-1);
      }
   }
