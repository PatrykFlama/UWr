interface IProps {
    id: string;
    children: React.ReactNode;
}

export default function Section({ id, children }: IProps) {
    return (
        <section id={id} className={`section ${id}`}>
            <div className="section-content">
                {children}
            </div>
        </section>
    );
}
