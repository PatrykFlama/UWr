import classes from './Section.module.scss';

interface IProps {
    id: string;
    children: React.ReactNode;
}

export default function Section({ id, children }: IProps) {
    return (
        <section id={id} className={`${classes['section']} ${id}`}>
            <div className={classes['section-content']}>
                {children}
            </div>
        </section>
    );
}
