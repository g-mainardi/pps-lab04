package u04.experimental;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.image.BufferedImage;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.function.Function;
import java.util.function.Supplier;

class CanvasFunctionalFacade {
    public static interface EventFacade { }

    public static class ClickOnCanvasEvent implements EventFacade {
        private final String canvasName;
        private final Point point;

        public ClickOnCanvasEvent(String canvasName, Point point) {
            this.canvasName = canvasName;
            this.point = point;
        }

        public String getCanvasName() {
            return canvasName;
        }

        public Point getPoint() {
            return point;
        }
    }

    public static class ComponentClick implements EventFacade {
        private final String componentName;

        public ComponentClick(String componentName) {
            this.componentName = componentName;
        }

        public String getComponentName() {
            return componentName;
        }
    }

    public static interface Frame {
        Frame setSize(int width, int height);
        Frame addButton(String text, String name);
        Frame addLabel(String text, String name);
        Frame showToLabel(String text, String name);
        Frame show();
        Supplier<EventFacade> events();
        Frame addCanvas(int width, int height, String name);
        Frame drawPixel(int x, int y, Color color, String name);
        Frame drawAllPixelsWith(Function<Point, Color> color, String name);
        Frame clearCanvas(String name);
    }

    public static Frame createFrame(){
        return new FrameImpl();
    }

    private static class FrameImpl implements Frame {
        private final JFrame jframe = new JFrame();
        private final Map<String, JButton> buttons = new HashMap<>();
        private final Map<String, JLabel> labels = new HashMap<>();
        private final Map<String, CanvasPanel> canvases = new HashMap<>();
        private final LinkedBlockingQueue<EventFacade> eventQueue = new LinkedBlockingQueue<>();

        private final Supplier<EventFacade> events = () -> {
            try{
                return eventQueue.take();
            } catch (InterruptedException e){
                return null; // it should never happen
            }
        };

        public FrameImpl() {
            this.jframe.setLayout(new FlowLayout());
        }

        @Override
        public Frame setSize(int width, int height) {
            this.jframe.setSize(width, height);
            return this;
        }

        @Override
        public Frame addButton(String text, String name) {
            JButton jb = new JButton(text);
            jb.setActionCommand(name);
            this.buttons.put(name, jb);
            jb.addActionListener(e -> {
                try {
                    eventQueue.put(new ComponentClick(name));
                } catch (InterruptedException ex){}
            });
            this.jframe.getContentPane().add(jb);
            return this;
        }

        @Override
        public Frame addLabel(String text, String name) {
            JLabel jl = new JLabel(text);
            this.labels.put(name, jl);
            this.jframe.getContentPane().add(jl);
            return this;
        }

        @Override
        public Supplier<EventFacade> events() {
            return events;
        }

        @Override
        public Frame showToLabel(String text, String name) {
            this.labels.get(name).setText(text);
            return this;
        }

        @Override
        public Frame show() {
            this.jframe.setVisible(true);
            return this;
        }

        @Override
        public Frame addCanvas(int width, int height, String name) {
            CanvasPanel canvas = new CanvasPanel(width, height);
            canvas.setPreferredSize(new Dimension(width, height));
            canvas.setBorder(BorderFactory.createLineBorder(Color.BLACK));


            // Add mouse listener for click events only
            canvas.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseClicked(MouseEvent e) {
                    try {
                        eventQueue.put(new ClickOnCanvasEvent(name, e.getPoint()));
                    } catch (InterruptedException ex) {}
                }
            });

            canvases.put(name, canvas);
            this.jframe.getContentPane().add(canvas);
            return this;
        }

        public Frame drawAllPixelsWith(Function<Point, Color> color, String name) {
            CanvasPanel canvas = canvases.get(name);
            if (canvas != null) {
                Graphics2D g = canvas.getGraphics();
                for (int x = 0; x < canvas.getWidth(); x++) {
                    for (int y = 0; y < canvas.getHeight(); y++) {
                        g.setColor(color.apply(new Point(x, y)));
                        g.fillRect(x, y, 1, 1);
                    }
                }
                g.dispose();
                jframe.repaint();
            }
            return this;
        }

        @Override
        public Frame drawPixel(int x, int y, Color color, String name) {
            CanvasPanel canvas = canvases.get(name);
            if (canvas != null) {
                Graphics2D g = canvas.getGraphics();
                g.setColor(color);
                g.fillRect(x, y, 1, 1);
                g.dispose();
                jframe.repaint();
            }
            return this;
        }

        @Override
        public Frame clearCanvas(String name) {
            CanvasPanel canvas = canvases.get(name);
            if (canvas != null) {
                canvas.clear();
                jframe.repaint();
            }
            return this;
        }
    }

    private static class CanvasPanel extends JPanel {
        private BufferedImage buffer;
        private Graphics graphics;

        public CanvasPanel(int width, int height) {
            buffer = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
            this.graphics = buffer.getGraphics();
            clear();
        }

        public Graphics2D getGraphics() {
            return (Graphics2D) buffer.getGraphics();
        }

        public void clear() {
            Graphics2D g = this.getGraphics();
            g.setColor(Color.WHITE);
            g.fillRect(0, 0, buffer.getWidth(), buffer.getHeight());
            g.dispose();
        }

        @Override
        protected void paintComponent(Graphics g) {
            super.paintComponent(g);
            g.drawImage(buffer, 0, 0, this);
        }
    }
}